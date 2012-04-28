-module(dk_ifi_cat).

-behavior(gen_server).

%% API
-export([name/2]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

name(DomId, CatId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ DomId ++ "][" ++ CatId ++ "]").

start_link(DomId, CatId) ->
    gen_server:start_link({local, name(DomId, CatId)}, ?MODULE,
                          [DomId, CatId], []).

%%% gen_server callbacks

init([_DomId, _CatId]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
