-module(dk_q).

%% API
-export([exec/3]).

%% Record declarations
-record(and_q, {subs}).
-record(or_q, {subs}).
-record(not_q, {sub}).
-record(kw_q, {kw}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

exec(DomId, CatId, QueryStr) ->
    {ok, Lang} = dk_meta:dom_lang(DomId),
    exec_q(DomId, CatId, from_str(QueryStr, Lang)).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

from_str(Str, Lang) ->
    {ok, ParseTree} = dk_q_parser:parse(scan(Str)),
    tree_to_query(ParseTree, Lang).

scan(<<C/utf8, Rest/bytes>>) ->
    case C of
        $( -> [{'(', 1} | scan(Rest)];
        $) -> [{')', 1} | scan(Rest)];
        $& -> [{'&', 1} | scan(Rest)];
        $| -> [{'|', 1} | scan(Rest)];
        $! -> [{'!', 1} | scan(Rest)];
        32 -> scan(Rest); % skip spaces
        _ -> 
            Regex = [<<"^([^()&|! ]*)(.*)$">>],
            Options = [unicode, global, {capture, all_but_first,
                                         binary}],
            case re:run(Rest, Regex, Options) of
                {match, [[Str, RestRest]]} ->
                    [{string, <<C, Str/bytes>>, 1} | scan(RestRest)];
                _ ->
                    [{string, <<C>>, 1} | scan(Rest)]
            end
    end;
scan(<<>>) ->
    [{'$end', 1}].

tree_to_query({and_q, SubTreeL, SubTreeR}, Lang) ->
    #and_q{subs = [tree_to_query(SubTreeL, Lang),
                   tree_to_query(SubTreeR, Lang)]};
tree_to_query({or_q, SubTreeL, SubTreeR}, Lang) ->
    #or_q{subs = [tree_to_query(SubTreeL, Lang),
                  tree_to_query(SubTreeR, Lang)]};
tree_to_query({not_q, SubTree}, Lang) ->
    #not_q{sub = tree_to_query(SubTree, Lang)};
tree_to_query({kw_q, {string, Keyword, _}}, Lang) ->
    [Term | _] = dk_pp:terms(Keyword, Lang),
    #kw_q{kw = Term}.

exec_q(DomId, CatId, Query) when is_record(Query, kw_q) ->
    get_posts_list(DomId, CatId, Query#kw_q.kw).
%% exec_q(IndexGid, Query) when is_record(Query, not_q) ->
%% exec_q(IndexGid, Query) when is_record(Query, and_q) ->
%% exec_q(IndexGid, Query) when is_record(Query, or_q) ->

get_posts_list(DomId, CatId, Term) ->
    Caller = self(),
    Tag = make_ref(),
    Receiver = posts_list_receiver(Caller, Tag, DomId, CatId, Term),
    Mref = monitor(process, Receiver),
    Receiver ! {self(), Tag},
    receive
        {'DOWN', Mref, _, _, {Receiver, Tag, Result}} ->
            Result;
        {'DOWN', Mref, _, _, Reason} ->
            %% receiver code failed
            exit(Reason)
    end.

posts_list_receiver(Caller, Tag, DomId, CatId, Term) ->
    spawn(
      fun() ->
              process_flag(trap_exit, true),
              Mref = monitor(process, Caller),
              receive
                  {Caller, Tag} ->
                      Keys = lists:map(
                               fun (Node) ->
                                       rpc:async_call(Node, dk_ii,
                                                      get_posts_list,
                                                      [DomId, CatId, Term])
                                                      
                               end,
                               dk_ring:whereis({invix_data, {DomId, CatId,
                                                             Term}})),
                      Result = yield(Keys, 1, length(Keys)),
                      exit({self(), Tag, Result});
                  {'DOWN', Mref, _, _, _} ->
                      %% caller died before sending us the go-ahead
                      exit(normal)
              end
      end).

yield(Keys, Index, Length = Index) ->
    case rpc:nb_yield(lists:nth(Index, Keys)) of
        {value, Value} -> Value;
        timeout -> yield(Keys, 1, Length)
    end;
yield(Keys, Index, Length) ->
    case rpc:nb_yield(lists:nth(Index, Keys)) of
        {value, Value} -> Value;
        timeout -> yield(Keys, Index + 1, Length)
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
