-module(dk_in).

%% API
-export([test/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

test(File) ->
    {ok, Binary} = file:read_file(File),
    add_doc(Binary),
    ok.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

add_doc(<<>>) ->
    ok;
add_doc(Binary) ->
    [Line, Rest] = binary:split(Binary, <<"\n">>), %% does this work correctly with UTF-8?
    [Id, Text] = binary:split(Line, <<" ">>),
    %% io:format("doc ~p~n", [Id]),
    IntId = list_to_integer(binary_to_list(Id)),
    plists:foreach(fun (T) -> dk_nii:add_post(T, IntId) end, dk_pp:terms(Text, "en")),
    add_doc(Rest).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
