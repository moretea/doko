-module(doko).

%% API
-export([start/0, start/1, stop/0]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% Starts a node in stand-alone mode.
start() ->
    application:set_env(doko, nodes, [node()]),
    application:start(doko).

%% Starts a node as part of a cluster.
start(Nodes) ->
    application:set_env(doko, nodes, Nodes),
    application:start(doko).

%% @doc Stops the node.
stop() ->
    application:stop(doko).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
