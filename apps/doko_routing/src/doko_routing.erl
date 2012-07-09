-module(doko_routing).

%% API
-export([whereto/1, wherefrom/1]).
-export([invix_data_id/2]).
-export_type([data_id/0]).

%% Type declarations
-opaque invix_data_id() :: {'invix',
                            doko_cluster:index_id(),
                            doko_utf8:str()}.
-type data_id() :: invix_data_id().

%% Macro definitions
-define(RING_SIZE, 420). % number of virtual nodes
-define(N_DUPS, 2). % number of duplicates

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec whereto(data_id()) -> [node(), ...].
whereto(DataId) ->
    where(DataId).

-spec wherefrom(data_id()) -> [node(), ...].
wherefrom(DataId) ->
    where(DataId).

-spec invix_data_id(doko_cluster:index_id(), doko_utf8:str()) -> data_id().
invix_data_id(IndexId, Term) ->
    {invix, IndexId, Term}.

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
