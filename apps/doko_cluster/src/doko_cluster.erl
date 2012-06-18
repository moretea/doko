-module(doko_cluster).

%% API
%% -export([add_doc/2]).
-export([start/1,stop/0]).
-export([whereto/1,wherefrom/1]).

-define(RING_SIZE, 420).
-define(N_DUPS, 2).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Adds a document.
%% add_doc(_DocId, _Text) ->

%% @doc Starts the application.
start(Nodes) ->
    %% TODO: check if number of nodes is greater than number of duplicates
    application:set_env(doko_cluster, nodes, Nodes),
    application:start(doko_cluster).

%% @doc Stops the application.
stop() ->
    application:stop(doko_cluster).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

whereto(Term) ->
    {ok, Nodes} = application:get_env(doko_cluster, nodes),
    lists:sublist(Nodes ++ Nodes, node_index(Term, Nodes), ?N_DUPS).

wherefrom(Term) ->
    {ok, Nodes} = application:get_env(doko_cluster, nodes),
    lists:nth(node_index(Term, Nodes), Nodes).

node_index(Term, Nodes) ->
    Vnode = erlang:phash2(Term, ?RING_SIZE),
    1 + erlang:trunc((Vnode / ?RING_SIZE) * length(Nodes)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
