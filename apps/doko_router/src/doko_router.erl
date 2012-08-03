-module(doko_router).
-compile({no_auto_import, [nodes/0]}).

-behaviour(gen_server).

%% API
-export([nodes/0, set_nodes/1]).
-export([to/1, from/1]).
-export([invix_data_id/2]).
-export_type([data_id/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Type declarations
-opaque invix_data_id() :: {'invix',
                            doko_cluster:index_id(),
                            doko_utf8:str()}.
-type data_id() :: invix_data_id().

%% Macro definitions
-define(RING_SIZE, 420). % number of virtual nodes
-define(N_DUPS, 2). % number of duplicates
-define(SERVER, ?MODULE).

%% Record definitions
-record(state, {nodes}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec nodes() -> [node(), ...].
nodes() ->
    {ok, Nodes} = application:get_env(doko_router, nodes),
    Nodes.

set_nodes(Nodes) ->
    %% TODO: check if number of nodes is at least equal to number of
    %% duplicates
    application:set_env(doko_router, nodes, Nodes).

-spec to(data_id()) -> [node(), ...].
to(DataId) ->
    where(DataId).

-spec from(data_id()) -> [node(), ...].
from(DataId) ->
    where(DataId).

-spec invix_data_id(doko_cluster:index_id(), doko_utf8:str()) -> data_id().
invix_data_id(IndexId, Term) ->
    {invix, IndexId, Term}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply,State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

where(DataId) ->
    Nodes = nodes(),
    Vnode = erlang:phash2(DataId, ?RING_SIZE),
    Start = 1 + erlang:trunc((Vnode / ?RING_SIZE) * length(Nodes)),
    lists:sublist(Nodes ++ Nodes, Start, ?N_DUPS).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
