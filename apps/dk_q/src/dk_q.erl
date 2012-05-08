-module(dk_q).

%% API
-export([from_string/1]).

%% Record declarations
-record(and_q, {subs}).
-record(or_q, {subs}).
-record(not_q, {sub}).
-record(kw_q, {kw}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

from_string(String) ->
    {ok, ParseTree} = dk_q_parser:parse(scan(String)),
    tree_to_query(ParseTree).

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
                {match, [[String, RestRest]]} ->
                    [{string, <<C, String/bytes>>, 1} | scan(RestRest)];
                _ ->
                    [{string, <<C>>, 1} | scan(Rest)]
            end
    end;
scan(<<>>) ->
    [{'$end', 1}].

tree_to_query({and_q, SubTreeL, SubTreeR}) ->
    #and_q{subs = [tree_to_query(SubTreeL), tree_to_query(SubTreeR)]};
tree_to_query({or_q, SubTreeL, SubTreeR}) ->
    #or_q{subs = [tree_to_query(SubTreeL), tree_to_query(SubTreeR)]};
tree_to_query({not_q, SubTree}) ->
    #not_q{sub = tree_to_query(SubTree)};
tree_to_query({kw_q, {string, Keyword, _}}) ->
    #kw_q{kw = Keyword}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
