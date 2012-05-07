-module(dk_node).

%% API
-export([start/0, stop/0]).

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
