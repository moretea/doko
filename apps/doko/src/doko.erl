-module(doko).

%% API
-export([start/0, start/1, stop/0]).
-export([add_dom/2, add_cat/2, add_doc/4]).
-export([run_query/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Starts a node in stand-alone mode.
start() ->
    start([node()]).

%% Starts a node as part of a cluster.
start(Nodes) ->
    application:set_env(doko, nodes, Nodes),
    application:start(doko).

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

%% @doc Adds a document.
add_doc(DomId, CatId, DocId, Doc) ->
    dk_in:add_doc(DomId, CatId, DocId, Doc).

%% @doc Executes a query.
run_query(DomId, CatId, Query) ->
    dk_q:exec(DomId, CatId, Query).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
