-module(doko_query).
-ifdef(DISABLED_FOR_NOW).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
%% -export([execute/2]).
-compile(export_all).

%% Record declarations ("q" is short for "query")
-record(and_q,  {l_sub_q :: q(), r_sub_q :: q()}).
-record(or_q,   {l_sub_q :: q(), r_sub_q :: q()}).
-record(not_q,  {sub_q :: q()}).
-record(term_q, {keyword :: doko_utf8:str(),
                 zone_ids :: list(doko_doc:zone_id())}).

%% Type definitions
-type q() :: {and_q,  q(), q()} |
             {or_q,   q(), q()} |
             {not_q,  q()     } |
             {term_q, doko_utf8:str(), list(doko_doc:zone_id())}.

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec execute(atom(), doko_utf8:str()) -> gb_set(). %% TODO: might return an error
execute(IndexId, Str) ->
    %% parse and preprocess query
    Clauses = [partition(flatten(X))||X <- clauses(dnf(from_str(Str)))],
    %% translate keywords to terms
    UniqueKeywords =
        lists:usort(lists:map(fun (X) -> element(1, X) end,
                              lists:flatten([Xs++Ys||{Xs,Ys} <- Clauses]))),
    Translate =
        fun (Keyword) ->
                Lang = doko_cluster:index_lang(IndexId),
                Result = case doko_preprocessing:uterms(Keyword, Lang) of
                             []    -> stop_word;
                             Terms -> Terms
                         end,
                {Keyword,Result}
        end,
    Terms = dict:from_list(lists:map(Translate, UniqueKeywords)),
    %% fetch data
    Fetch = fun (Term) -> {Term, doko_cluster:doc_ids(IndexId, Term)} end,
    UniqueTerms =
        lists:flatten(
          [Value||{_Key,Value} <- dict:to_list(Terms), Value /= stop_word]),
    Data = dict:from_list(plists:map(Fetch, UniqueTerms)),
    %% calculate result
    DocIdsForZoneIds =
        fun (ZoneIds, Dict) ->
                case ZoneIds of
                    [] ->
                        dict:fetch(any, Dict);
                    _ ->
                        gb_sets:union(
                          [element(2,Y) || X <- ZoneIds,
                                           Y <- dict:find(X, Dict),
                                           Y /= error])
                end
        end,
    DocIds =
        fun ({Keyword, ZoneIds}) ->
                io:format("Keyword = ~p~n", [Keyword]),
                io:format("ZoneIds = ~p~n", [ZoneIds]),
                case dict:fetch(Keyword, Terms) of
                    stop_word -> gb_sets:empty();
                    Term -> DocIdsForZoneIds(ZoneIds, dict:fetch(Term, Data))
                end
        end,
    Calculate = fun ({Keywords,NotKeywords}) ->
                        gb_sets:subtract(
                          gb_sets:intersection(
                            lists:flatten([DocIds(X)||X <- Keywords])),
                          gb_sets:union(
                            lists:flatten([DocIds(X)||X <- NotKeywords])))
                end,
    gb_sets:union(plists:map(Calculate, Clauses)).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

%% @doc Converts a UTF-8 string to a query. Returns a query record plus the
%% depth of the corresponding tree.
-spec from_str(doko_utf8:str()) -> {q(),pos_integer()}.
from_str(Str) ->
    {ok,ParseTree} = doko_query_parser:parse(scan(Str)),
    tree_to_query(ParseTree).

scan(<<>>) ->
    [{'$end',1}];
scan(<<C/utf8,Rest/bytes>>) ->
    case C of
        %% parentheses
        $( -> [{'(',1}|scan(Rest)];
        $) -> [{')',1}|scan(Rest)];
        $[ -> [{'[',1}|scan(Rest)];
        $] -> [{']',1}|scan(Rest)];
        %% boolean operators
        $& -> [{'&',1}|scan(Rest)];
        $| -> [{'|',1}|scan(Rest)];
        $! -> [{'!',1}|scan(Rest)];
        %% comma
        $, -> [{',', 1} | scan(Rest)];
        %% space (ignored)
        32 -> scan(Rest);
        %% begin of a single quoted string
        $' ->
            Regex = [<<"^((?:[^'\\\\]|\\\\')*)'(.*)$">>],
            Options = [unicode,{capture,all_but_first,binary}],
            case re:run(Rest, Regex, Options) of
                {match,[String,RestRest]} ->
                    [{string,String,1}|scan(RestRest)]
            end;
        %% begin of a keyword or an ID
        _ ->
            Regex = [<<"^([a-z0-9_]*)(.*)$">>],
            Options = [unicode,{capture,all_but_first,binary}],
            case re:run(<<C/utf8,Rest/bytes>>, Regex, Options) of
                {match,[Word,RestRest]} ->
                    Token = case Word of
                                <<"in">> -> {'in',1};
                                Id       -> {id,Id,1}
                            end,
                    [Token|scan(RestRest)]
            end
    end.

tree_to_query({and_q,SubTreeL,SubTreeR}) ->
    {L, DepthL} = tree_to_query(SubTreeL),
    {R, DepthR} = tree_to_query(SubTreeR),
    {#and_q{l_sub_q = L,r_sub_q = R},max(DepthL, DepthR)+1};
tree_to_query({or_q,SubTreeL,SubTreeR}) ->
    {L, DepthL} = tree_to_query(SubTreeL),
    {R, DepthR} = tree_to_query(SubTreeR),
    {#or_q{l_sub_q = L,r_sub_q = R},max(DepthL, DepthR)+1};
tree_to_query({not_q,SubTree}) ->
    {Sub,Depth} = tree_to_query(SubTree),
    {#not_q{sub_q = Sub},Depth+1};
tree_to_query({term_q,{string,Keyword,_},ZoneIds}) ->
    Ids = [binary:bin_to_list(Id)||{_,Id,_} <- ZoneIds],
    {#term_q{keyword  = Keyword,zone_ids = Ids},0}.

%% @doc Rewrites a query to disjunctive normal form.
-spec dnf({q(),pos_integer()}) -> q().
dnf({Q,0}) ->
    Q;
dnf({Q,D}) ->
    dnf(mv_not(Q), D).

dnf(Q, 1) ->
    Q;
dnf(Q, D) ->
    dnf(mv_and(Q), D-1).

mv_not(#and_q{l_sub_q = L,r_sub_q = R}) ->
    #and_q{l_sub_q = mv_not(L),
           r_sub_q = mv_not(R)};
mv_not(#or_q{l_sub_q = L,r_sub_q = R}) ->
    #or_q{l_sub_q = mv_not(L),
          r_sub_q = mv_not(R)};
mv_not(#not_q{sub_q = #and_q{l_sub_q = L,r_sub_q = R}}) ->
    #or_q{l_sub_q = mv_not(#not_q{sub_q = L}),
          r_sub_q = mv_not(#not_q{sub_q = R})};
mv_not(#not_q{sub_q = #or_q{l_sub_q = L,r_sub_q = R}}) ->
    #and_q{l_sub_q = mv_not(#not_q{sub_q = L}),
           r_sub_q = mv_not(#not_q{sub_q = R})};
mv_not(#not_q{sub_q = #not_q{sub_q = Q}}) ->
    mv_not(Q);
mv_not(Q) ->
    Q.

mv_and(#and_q{l_sub_q = Q, r_sub_q = #or_q{l_sub_q = L, r_sub_q = R}}) ->
    #or_q{l_sub_q = #and_q{l_sub_q = Q, r_sub_q = mv_and(L)},
          r_sub_q = #and_q{l_sub_q = Q, r_sub_q = mv_and(R)}};
mv_and(#and_q{l_sub_q = #or_q{l_sub_q = L, r_sub_q = R}, r_sub_q = Q}) ->
    #or_q{l_sub_q = #and_q{l_sub_q = Q, r_sub_q = mv_and(L)},
          r_sub_q = #and_q{l_sub_q = Q, r_sub_q = mv_and(R)}};
mv_and(#or_q{l_sub_q = L, r_sub_q = R}) ->
    #or_q{l_sub_q = mv_and(L), r_sub_q = mv_and(R)};
mv_and(Q) ->
    Q.

%% @doc Converts a query in DNF to a lists of AND queries.
clauses(#or_q{l_sub_q = L, r_sub_q = R}) ->
    clauses(L) ++ clauses(R);
clauses(Q) ->
    [Q].

%% @doc Converts an AND clause to  a list of TERM and NOT queries.
flatten(#and_q{l_sub_q = L, r_sub_q = R}) ->
    lists:flatten([flatten(L),flatten(R)]);
flatten(Q) ->
    [Q].

%% @doc Partitions a list of TERM and NOT queries into two lists, where the
%% first lists contains all terms and zone IDs in TERM queries and the second
%% list contains all terms and zone IDs in NOT queries.
partition(Qs) ->
    {TermQs,NotQs} = lists:partition(fun (Q) -> is_record(Q, term_q) end, Qs),
    Destruct = fun (Q) ->
                       case Q of
                           #not_q{sub_q = S} ->
                               {S#term_q.keyword,S#term_q.zone_ids};
                           _ ->
                               {Q#term_q.keyword,Q#term_q.zone_ids}
                       end
               end,
    {lists:usort([Destruct(Q)||Q <- TermQs]),
     lists:usort([Destruct(Q)||Q <- NotQs])}.

%%----------------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------------

-ifdef(DISABLED_FOR_NOW).

proper_test_() ->
    [{atom_to_list(F),
      fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].

prop_all_not_element() ->
    ?FORALL(X, q(), not_element(dnf({X,depth(X)}))).

not_element(#term_q{}) ->
    true;
not_element({_, L, R}) ->
    not_element(L) and not_element(R);
not_element(#not_q{sub_q = #term_q{}}) ->
    true;
not_element(#not_q{}) ->
    false.

depth({_,L,R}) ->
    max(depth(L),depth(R))+1;
depth({not_q,Q}) ->
    depth(Q)+1;
depth(_) ->
    0.

prop_no_nested_or() ->
    ?FORALL(X, q(), not nested_or(dnf({X,depth(X)}))).

nested_or({q,L,R}) ->
    (is_record(L, or_q) or is_record(R, or_q))
        orelse (nested_or(L) or nested_or(R));
nested_or({or_q,L,R}) ->
    nested_or(L) or nested_or(R);
nested_or({not_q,Q}) ->
    nested_or(Q);
nested_or(_) ->
    false.

-endif.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
