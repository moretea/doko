-module(dk_q).
-include_lib("proper/include/proper.hrl").

%% API
-export([execute/1]).

%% Record declarations ("q" is short for "query")
-record(and_q,  {l_sub_q :: q(), r_sub_q :: q()}).
-record(or_q,   {l_sub_q :: q(), r_sub_q :: q()}).
-record(not_q,  {sub     :: q()}).
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
    Clauses = [partition(flatten(X)) || X <- and_subs(dnf(from_str(Str)))],
    %% fetch data
    Data = plists:mapreduce(
             fun fetch/1, 
             lists:usort(lists:flatten([Xs++Ys || {Xs,Ys} <- Clauses]))),
    %% calculate result
    gb_sets:union(plists:map(fun (X) -> calc(X, Data) end, Clauses)).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

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
    {#not_q{sub = Sub},Depth+1};
tree_to_query({term_q,{string,Keyword,_}}) ->
    {#term_q{keyword = Keyword},0}.

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

and_subs({or_q,L,R}) ->
    and_subs(L) ++ and_subs(R);
and_subs(Q) ->
    [Q].

flatten({and_q,L,R}) ->
    lists:flatten([flatten(L),flatten(R)]);
flatten(Q) ->
    [Q].

partition(Qs) ->
    {Pos,Neg} = lists:partition(fun (Q) -> is_record(Q, term_q) end, Qs),
    {terms(Pos),terms(Neg)}.

terms(Qs) ->
    lists:usort([term(Q) || Q <- Qs]).

term({not_q,{term_q,T}}) ->
    T;
term({term_q,T}) ->
    T.

fetch(K) ->
    {K, case dk_pp:terms(K, "en") of                   % FIXME: hardcoded lang
            [] ->
                stop_word;
            [T|[]] ->
                dk_idx:doc_ids(T);
            Ts ->
                gb_sets:intersection(plists:map(fun dk_idx:doc_ids/1, Ts))
        end}.

calc({Ts,Ns}, Data) ->
    case [S || T <- Ts, S <- dict:fetch(T, Data), S /= stop_word] of
        [] ->
            gb_sets:new();
        Ss ->
            gb_sets:subtract(
              gb_sets:intersection(Ss),
              gb_sets:union(
                [S || N <- Ns, S <- dict:fetch(N, Data), S /= stop_word]))
    end.

%%----------------------------------------------------------------------------
%% PropErties
%%----------------------------------------------------------------------------

prop_all_not_element() ->
    ?FORALL(X, q(), not_element(dnf({X,depth(X)}))).

not_element({_, L, R}) ->
    not_element(L) and not_element(R);
not_element(#not_q{sub = #term_q{}}) ->
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

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
