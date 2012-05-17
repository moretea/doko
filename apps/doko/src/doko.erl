-module(doko).

%% API
-export([start/1, stop/0]).
-export([add_dom/2, add_cat/2, add_doc/4]).
-export([run_query/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% Starts a node as part of a cluster.
start(Nodes) ->
    application:set_env(doko, nodes, Nodes),
    application:start(doko).

%% @doc Stops the node.
stop() ->
    application:stop(doko).

%% @doc Adds a domain.
add_dom(DomId, Lang) ->
    Nodes = dk_ring:nodes(),
    %% TO-DO: handle errors
    Result = lists:duplicate(length(Nodes), ok),
    {Result, []} = rpc:multicall(Nodes, dk_meta, add_dom, [DomId, Lang]),
    {Result, []} = rpc:multicall(Nodes, dk_ii, add_dom, [DomId]),
    ok.

%% @doc Adds a category.
add_cat(DomId, CatId) ->
    Nodes = dk_ring:nodes(),
    %% TO-DO: handle errors
    Result = lists:duplicate(length(Nodes), ok),
    {Result, []} = rpc:multicall(Nodes, dk_ii, add_cat, [DomId, CatId]),
    ok.

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
