-module(dk_q).
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([execute/1]).

%% Record declarations ("q" is short for "query")
-record(and_q,  {l_sub_q :: q(), r_sub_q :: q()}).
-record(or_q,   {l_sub_q :: q(), r_sub_q :: q()}).
-record(not_q,  {sub_q   :: q()}).
-record(term_q, {keyword :: utf8_str()}).

%% Type definitions
-type q() :: {and_q,  q(), q()} |
             {or_q,   q(), q()} |
             {not_q,  q()     } |
             {term_q, utf8_str()}.
-type utf8_str() :: unicode:unicode_binary().

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec execute(utf8_str()) -> gb_set(). %% TODO: might return an error
execute(Str) ->
    %% parse and preprocess query
    Clauses = [partition(flatten(X))||X <- clauses(dnf(from_str(Str)))],
    %% translate keywords to terms
    UniqueKeywords =
        lists:usort(lists:flatten([Xs++Ys||{Xs,Ys} <- Clauses])),
    Translate = fun (Keyword) ->
                        %% FIXME: hardcoded language
                        Result = case dk_pp:terms(Keyword, "en") of
                                     []    -> stop_word;
                                     Terms -> Terms
                                 end,
                        {Keyword,Result}
                end,
    Terms = dict:from_list(lists:map(Translate, UniqueKeywords)),
    %% fetch data
    Fetch = fun (Term) ->
                    DocIds = dk_idx:doc_ids(Term),
                    {Term,DocIds}
            end,
    UniqueTerms = 
        lists:flatten(
          [Value||{_Key,Value} <- dict:to_list(Terms), Value /= stop_word]),
    Data = dict:from_list(plists:map(Fetch, UniqueTerms)),
    %% calculate result
    DocIds = fun (Keyword) ->
                     case dict:fetch(Keyword, Terms) of
                         stop_word ->
                             gb_sets:new();
                         Result ->
                             [dict:fetch(X,Data)||X <- Result]
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
-spec from_str(utf8_str()) -> {q(),pos_integer()}.
from_str(Str) ->
    {ok,ParseTree} = dk_q_parser:parse(scan(Str)),
    tree_to_query(ParseTree).

scan(<<C/utf8,Rest/bytes>>) ->
    case C of
        $( -> [{'(',1}|scan(Rest)];
        $) -> [{')',1}|scan(Rest)];
        $& -> [{'&',1}|scan(Rest)];
        $| -> [{'|',1}|scan(Rest)];
        $! -> [{'!',1}|scan(Rest)];
        32 -> scan(Rest); % skip spaces
        _  ->
            Regex = [<<"^([^()&|! ]*)(.*)$">>],
            Options = [unicode,global,{capture,all_but_first,binary}],
            case re:run(Rest, Regex, Options) of
                {match,[[Str,RestRest]]} ->
                    [{string,<<C,Str/bytes>>,1}|scan(RestRest)];
                _ ->
                    [{string,<<C>>,1}|scan(Rest)]
            end
    end;
scan(<<>>) ->
    [{'$end',1}].

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
tree_to_query({term_q,{string,Keyword,_}}) ->
    {#term_q{keyword = Keyword},0}.

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

mv_not({T,L,R}) ->
    {T,mv_not(L),mv_not(R)};
mv_not({not_q,{and_q,L,R}}) ->
    {or_q,mv_not({not_q,L}),mv_not({not_q,R})};
mv_not({not_q,{or_q,L,R}}) ->
    {and_q,mv_not({not_q,L}),mv_not({not_q,R})};
mv_not({not_q,{not_q,Q}}) ->
    mv_not(Q);
mv_not(Q) ->
    Q.

mv_and({and_q,Q,{or_q,L,R}}) ->
    {or_q,mv_and({and_q,Q,mv_and(L)}),mv_and({and_q,Q,mv_and(R)})};
mv_and({and_q,{or_q,L,R},Q}) ->
    {or_q,mv_and({and_q,Q,mv_and(L)}),mv_and({and_q,Q,mv_and(R)})};
mv_and({T,L,R}) ->
    {T,mv_and(L),mv_and(R)};
mv_and(Q) ->
    Q.

%% @doc Converts a query in DNF to a lists of AND queries.
clauses({or_q,L,R}) ->
    clauses(L) ++ clauses(R);
clauses(Q) ->
    [Q].

%% @doc Converts an AND clause to  a list of TERM and NOT queries.
flatten({and_q,L,R}) ->
    lists:flatten([flatten(L),flatten(R)]);
flatten(Q) ->
    [Q].

%% @doc Partitions a list of TERM and NOT queries into two lists, where the
%% first lists contains all terms in TERM queries and the second list contains
%% all terms in NOT queries.
partition(Qs) ->
    {TermQs,NotQs} =
        lists:partition(fun (Q) -> is_record(Q, term_q) end, Qs),
    {keywords(TermQs),keywords(NotQs)}.

keywords(Qs) ->
    lists:usort([keyword(Q)||Q <- Qs]).

keyword(Q = #not_q{}) ->
    Q#not_q.sub_q#term_q.keyword;
keyword(Q) ->
    Q#term_q.keyword.

%%----------------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------------

-ifdef(TEST).

proper_test_() ->
    [{atom_to_list(F),
      fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].

prop_all_not_element() ->
    ?FORALL(X, q(), not_element(dnf({X,depth(X)}))).

not_element({_, L, R}) ->
    not_element(L) and not_element(R);
not_element(#not_q{sub_q = #term_q{}}) ->
    true;
not_element(#not_q{}) ->
    false;
not_element(#term_q{}) ->
    true.

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
