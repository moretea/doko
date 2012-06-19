-module(doko_cluster).

%% API
-export([add_doc/2,doc_ids/1]).
-export([start/1,stop/0]).

-define(RING_SIZE, 420). % number of virtual nodes
-define(N_DUPS, 2). % number of duplicates

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Adds a document.
add_doc(DocId, Terms) ->
    AddDocId =
        fun (Term) ->
                %% TODO: choose appropriate timeout
                Timeout = infinity,
                %% TODO: handle errors
                {_,[]} = rpc:multicall(write_nodes(Term),
                                       doko_node, add_doc_id, [Term, DocId],
                                       Timeout)
        end,
    plists:foreach(AddDocId, Terms).

doc_ids(Term) ->
    %% TODO: choose appropriate timeout
    Timeout = infinity,
    %% TODO: handle errors
    rpc:call(read_node(Term), doko_node, doc_ids, [Term], Timeout).

%% @doc Starts the application.
start(Nodes) ->
    %% TODO: check if number of nodes is at least equal to number of
    %% duplicates
    application:set_env(doko_cluster, nodes, Nodes),
    application:start(doko_cluster).

%% @doc Stops the application.
stop() ->
    application:stop(doko_cluster).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

write_nodes(Term) ->
    {ok, Nodes} = application:get_env(doko_cluster, nodes),
    lists:sublist(Nodes ++ Nodes, node_index(Term, Nodes), ?N_DUPS).

read_node(Term) ->
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
