-module(dk_q).

%% API
%% -export([exec/3]).
-compile(export_all).

%% Record declarations
-record(and_q, {subs}).
-record(or_q, {subs}).
-record(not_q, {sub}).
-record(term_q, {term}).

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

dnf(Q = #term_q{}) ->
    Q;
dnf(Q = #not_q{sub = #term_q{}}) ->
    Q;
dnf(#not_q{sub = #not_q{sub = Q}}) ->
    dnf(Q);
dnf(#not_q{sub = #or_q{subs = Qs}}) ->
    dnf(#and_q{subs = [#not_q{sub = dnf(Q)} || Q <- Qs]});
dnf(#not_q{sub = #and_q{subs = Qs}}) ->
    #or_q{subs = [#not_q{sub = dnf(Q)} || Q <- Qs]};
dnf(#or_q{subs = Qs}) ->
    #or_q{subs = [dnf(Q) || Q <- Qs]};
dnf(#and_q{subs = Qs}) ->
    Fun = fun(Q) -> case Q of
                        #term_q{} -> [Q];
                        #not_q{} -> [Q];
                        #or_q{subs = Rs} -> Rs
                    end
          end,
    Product = product([Fun(dnf(Q)) || Q <- Qs]),
    #or_q{subs = [#and_q{subs = Rs} || Rs <- Product]}.

product([Xs, Ys | Rest]) ->
    product(Rest, [[X, Y] || X <- Xs, Y <- Ys]).

product([], Acc) ->
    Acc;
product([L], Acc) ->
    [[H | T] || H <- L, T <- Acc];
product([L | Rest], Acc) ->
    product(Rest, [[H | T] || H <- L, T <- Acc]).

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
tree_to_query({term_q, {string, Keyword, _}}, Lang) ->
    [Term | _] = dk_pp:terms(Keyword, Lang),
    #term_q{term = Term}.

exec_q(DomId, CatId, #term_q{term = Term}) ->
    get_posts(DomId, CatId, Term);
exec_q(DomId, CatId, #and_q{subs = Subs}) ->
    sets:intersection([exec_q(DomId, CatId, Sub) || Sub <- Subs]);
exec_q(DomId, CatId, #or_q{subs = Subs}) ->
    sets:union([exec_q(DomId, CatId, Sub) || Sub <- Subs]);
exec_q(DomId, CatId, #not_q{sub = Sub}) ->
    sets:subtract(get_doc_ids(DomId, CatId), exec_q(DomId, CatId, Sub)).

get_posts(DomId, CatId, Term) ->
    Nodes = dk_ring:whereis({invix_data, {DomId, CatId, Term}}),
    get_data(Nodes, get_posts, [DomId, CatId, Term]).

get_doc_ids(DomId, CatId) ->
    Nodes = dk_ring:whereis({cat_data, {DomId, CatId}}),
    get_data(Nodes, get_doc_ids, [DomId, CatId]).

get_data(Nodes, Fun, Args) ->
    Caller = self(),
    Tag = make_ref(),
    Receiver = receiver(Caller, Tag, Nodes, Fun, Args),
    Mref = monitor(process, Receiver),
    Receiver ! {self(), Tag},
    receive
        {'DOWN', Mref, _, _, {Receiver, Tag, Result}} ->
            Result;
        {'DOWN', Mref, _, _, Reason} ->
            %% receiver code failed
            exit(Reason)
    end.

receiver(Caller, Tag, Nodes, Fun, Args) ->
    spawn(
      fun() ->
              process_flag(trap_exit, true),
              Mref = monitor(process, Caller),
              receive
                  {Caller, Tag} ->
                      AsyncCall = 
                          fun (Node) ->
                                  rpc:async_call(Node, dk_ii, Fun, Args)
                          end,
                      Keys = lists:map(AsyncCall, Nodes),
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
