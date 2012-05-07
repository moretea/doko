-module(dk_ring).

-behaviour(gen_server).

%% API
-export([whereis/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macro definitions
-define(SERVER, ?MODULE).
-define(RING_SIZE, 420). % 420 has many divisors
-define(N_DUPS, 2). % number of duplicates (i.e. replication "level")

%% Record declarations
-record(ring, {map :: [{non_neg_integer(), node()}, ...]}).
-record(state, {ring}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Returns the nodes where data is located.
whereis(DataId) ->
    gen_server:call(?SERVER, {whereis, DataId}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, Nodes} = application:get_env(doko, nodes),
    Step = ?RING_SIZE / length(Nodes),
    Indices = lists:map(fun (N) -> round(N * Step) end,
                        lists:seq(0, length(Nodes) - 1)),
    Ring = #ring{map = lists:zip(Indices, Nodes)},
    {ok, #state{ring=Ring}}.

%% @private
handle_call({whereis, DataId}, _From, State) ->
    Ring = State#state.ring,
    Reply = whereis(Ring, DataId),
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

whereis(Ring, DataId) ->
    Vnode = erlang:phash2(DataId, ?RING_SIZE),
    Dict = orddict:filter(fun (Index, _Node) -> Index > Vnode end,
                          Ring#ring.map),
    Nodes = fetch_values(Dict) ++ fetch_values(Ring#ring.map),
    Size = length(Ring#ring.map),
    case Size < ?N_DUPS of
        true -> lists:sublist(Nodes, Size);
        false -> lists:sublist(Nodes, ?N_DUPS)
    end.

fetch_values(Dict) ->
    lists:map(fun (Key) -> orddict:fetch(Key, Dict) end,
              orddict:fetch_keys(Dict)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
