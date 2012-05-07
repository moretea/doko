-module(dk_node).

%% API
-export([start/0, stop/0]).
-export([add_dom/2, add_cat/2]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Starts the node.
start() ->
    application:start(dk_node).

%% @doc Stops the node.
stop() ->
    application:stop(node).

%% @doc Adds a domain.
add_dom(DomId, Lang) ->
    dk_meta:add_dom(DomId, Lang),
    dk_ii:add_dom(DomId).

%% @doc Adds a category.
add_cat(DomId, CatId) ->
    dk_ii:add_cat(DomId, CatId).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
