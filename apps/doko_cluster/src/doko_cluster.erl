-module(doko_cluster).

%% API
-export([start/1, stop/0]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% Starts as part of a cluster.
start(Nodes) ->
    application:set_env(doko_cluster, nodes, Nodes),
    application:start(doko_cluster).

%% @doc Stops the application.
stop() ->
    application:stop(doko_cluster).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
