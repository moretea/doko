-module(dk_node).

%% API
-export([start/1, stop/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Starts the node.
start() ->
    application:start(dk_node).

%% @doc Stops the node.
stop() ->
    application:stop(node).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
