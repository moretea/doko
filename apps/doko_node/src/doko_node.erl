-module(doko_node).

%% API
-export([start/0, start/1, stop/0]).
-export([load/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% Starts a node in stand-alone mode.
start() ->
    application:set_env(doko_node, nodes, [node()]),
    application:start(doko_node).

%% Starts a node as part of a cluster.
start(Nodes) ->
    application:set_env(doko_node, nodes, Nodes),
    application:start(doko_node).

%% @doc Stops the node.
stop() ->
    application:stop(doko_node).

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
