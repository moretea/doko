-module(dk_q).
-include_lib("proper/include/proper.hrl").

%% API
-export([clauses/1]).

%% Record declarations
-record(and_q,  {l_sub :: q(), r_sub :: q()}).
-record(or_q,   {l_sub :: q(), r_sub :: q()}).
-record(not_q,  {sub   :: q()}).
-record(term_q, {kw    :: utf8_str()}).

%% Type definitions
-type q() :: {and_q,  q(), q()} |
             {or_q,   q(), q()} |
             {not_q,  q()     } |
             {term_q, utf8_str()}.
-type utf8_str() :: unicode:unicode_binary().

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

clauses(Str) ->
    [flatten(X) || X <- and_subs(dnf(from_str(Str)))].

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
    {#and_q{l_sub = L,r_sub = R},max(DepthL, DepthR)+1};
tree_to_query({or_q,SubTreeL,SubTreeR}) ->
    {L, DepthL} = tree_to_query(SubTreeL),
    {R, DepthR} = tree_to_query(SubTreeR),
    {#or_q{l_sub = L,r_sub = R},max(DepthL, DepthR)+1};
tree_to_query({not_q,SubTree}) ->
    {Sub,Depth} = tree_to_query(SubTree),
    {#not_q{sub = Sub},Depth+1};
tree_to_query({term_q,{string,Keyword,_}}) ->
    {#term_q{kw = Keyword},0}.

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
