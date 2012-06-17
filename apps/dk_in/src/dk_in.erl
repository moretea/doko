-module(dk_in).

%% API
-export([load/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

load(File) ->
    {ok, Binary} = file:read_file(File),
    add_docs(Binary),
    ok.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

add_docs(<<>>) ->
    ok;
add_docs(Binary) ->
    [Line, Rest] = binary:split(Binary, <<"\n">>), % does this work correctly
                                                   % with UTF-8?
    [Id, Text] = binary:split(Line, <<" ">>),
    IntId = list_to_integer(binary_to_list(Id)),
    plists:foreach(fun (T) -> doko_index:add_doc_id(T, IntId) end,
                   dk_pp:terms(Text, "en")),
    add_docs(Rest).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
