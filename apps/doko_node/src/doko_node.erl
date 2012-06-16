-module(doko_node).

%% API
-export([start/0, start/1, stop/0]).

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

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
