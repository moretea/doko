-module(doko_routing).

%% API
-export([whereto/1, wherefrom/1]).

%% Macro definitions
-define(RING_SIZE, 420). % number of virtual nodes
-define(N_DUPS, 2). % number of duplicates

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

whereto(DataId) ->
    where(DataId).

wherefrom(DataId) ->
    where(DataId).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

where(DataId) ->
    {ok,Nodes} = application:get_env(doko_cluster, nodes),
    Vnode = erlang:phash2(DataId, ?RING_SIZE),
    Start = 1 + erlang:trunc((Vnode / ?RING_SIZE) * length(Nodes)),
    lists:sublist(Nodes ++ Nodes, Start, ?N_DUPS).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
