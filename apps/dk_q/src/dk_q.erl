-module(dk_q).

%% API
-compile(export_all).

%% Record declarations
-record(and_q, {subs}).
-record(or_q, {subs}).
-record(not_q, {sub}).
-record(term_q, {term}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

from_str(Str, Lang) ->
    {ok, ParseTree} = dk_q_parser:parse(scan(Str)),
    compress(tree_to_query(ParseTree, Lang)).

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
    case dk_pp:terms(Keyword, Lang) of
        [] ->
            #or_q{subs = []};
        [Term | _]  ->
            #term_q{term = Term}
    end.

compress(Q = #term_q{}) ->
    Q;
compress(Q = #not_q{}) ->
    Q;
compress(#and_q{subs = Qs}) ->
    {Ands, Rest} = lists:partition(fun (X) -> is_record(X, and_q) end,
                                   [compress(Q) || Q <- Qs]),
    #and_q{subs = lists:flatten([subs(Q) || Q <- Ands]) ++ Rest};
compress(#or_q{subs = Qs}) ->
    {Ors, Rest} = lists:partition(fun (X) -> is_record(X, or_q) end,
                                  [compress(Q) || Q <- Qs]),
    #or_q{subs = lists:flatten([subs(Q) || Q <- Ors]) ++ Rest}.

subs(Q = #term_q{}) ->
    [Q];
subs(Q = #not_q{}) ->
    [Q];
subs(#or_q{subs = Qs}) ->
    Qs;
subs(#and_q{subs = Qs}) ->
    Qs.

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
    Product = product([subs(dnf(Q)) || Q <- Qs]),
    #or_q{subs = [#and_q{subs = Rs} || Rs <- Product]}.

product([Xs, Ys | Rest]) ->
    product(Rest, [[X, Y] || X <- Xs, Y <- Ys]).

product([], Acc) ->
    Acc;
product([L], Acc) ->
    [[H | T] || H <- L, T <- Acc];
product([L | Rest], Acc) ->
    product(Rest, [[H | T] || H <- L, T <- Acc]).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
