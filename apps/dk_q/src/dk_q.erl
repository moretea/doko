-module(dk_q).

%% API
-export([from_str/2]).
-export([dnf/1]).

%% Record declarations
-record(and_q, {subs :: [q(),...]}).
-record(or_q, {subs :: [q(),...]}).
-record(not_q, {sub :: q()}).
-record(term_q, {term :: utf8_str()}).

%% Type definitions
-type q() :: #and_q{} | #or_q{} | #not_q{} | #term_q{}.
-type utf8_str() :: unicode:unicode_binary().

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% convert string to query
from_str(Str, Lang) ->
    {ok, ParseTree} = dk_q_parser:parse(scan(Str)),
    tree_to_query(ParseTree, Lang).

%% rewrite query to DNF
dnf(Q) ->
    denest(distr_and(denest(mv_not_in(Q)))).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

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

mv_not_in(#and_q{subs = Qs}) ->
    #and_q{subs = [mv_not_in(Q) || Q <- Qs]};
mv_not_in(#or_q{subs = Qs}) ->
    #or_q{subs = [mv_not_in(Q) || Q <- Qs]};
mv_not_in(#not_q{sub = #and_q{subs = Qs}}) ->
    #or_q{subs = [mv_not_in(#not_q{sub = Q}) || Q <- Qs]};
mv_not_in(#not_q{sub = #or_q{subs = Qs}}) ->
    #and_q{subs = [mv_not_in(#not_q{sub = Q}) || Q <- Qs]};
mv_not_in(#not_q{sub = #not_q{sub = Q}}) ->
    mv_not_in(Q);
mv_not_in(Q = #not_q{sub = #term_q{}}) ->
    Q;
mv_not_in(Q = #term_q{}) ->
    Q.

denest(#and_q{subs = Qs}) ->
    Rs = lists:flatten(lists:map(fun denest/1, Qs)),
    {Ands, Rest} = lists:partition(fun (R) -> is_record(R, and_q) end, Rs),
    #and_q{subs = lists:flatmap(fun subs/1, Ands) ++ Rest};
denest(#or_q{subs = Qs}) ->
    Rs = lists:map(fun denest/1, Qs),
    {Ors, Rest} = lists:partition(fun (R) -> is_record(R, or_q) end, Rs),
    #or_q{subs = lists:flatmap(fun subs/1, Ors) ++ Rest};
denest(#not_q{sub = Q}) ->
    #not_q{sub = denest(Q)};
denest(Q = #term_q{}) ->
    Q.

distr_and(And = #and_q{subs = Qs}) ->
    case lists:partition(fun (Q) -> is_record(Q, or_q) end, Qs) of
        {[], _Rest} ->
            And;
        {Ors, Rest} ->
            #or_q{subs = [distr_and(#and_q{subs = Rs})
                          || Rs <- product([Q#or_q.subs || Q <-Ors],
                                           [Rest])]}
    end;
distr_and(#or_q{subs = Qs}) ->
    #or_q{subs = [distr_and(Q) || Q <- Qs]};
distr_and(Q = #not_q{}) ->
    Q;
distr_and(Q = #term_q{}) ->
    Q.

subs(#and_q{subs = Qs}) ->
    Qs;
subs(#or_q{subs = Qs}) ->
    Qs.

product([], Acc) ->
    Acc;
product([L | Rest], Acc) ->
    product(Rest, [[H | T] || H <- L, T <- Acc]).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
