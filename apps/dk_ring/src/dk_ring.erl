-module(dk_ring).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macro definitions
-define(SERVER, ?MODULE).
-define(RING_SIZE, 420). % 420 has many divisors

%% Record declarations
-record(ring, {map :: [{non_neg_integer(), node()}, ...]}).
-record(state, {ring}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

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
handle_call(_Msg, _From, State) ->
    Reply = ok,
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

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
