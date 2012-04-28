-module(dk_ifi).

-behavior(gen_server).

%% API
-export([add_dom/1, del_dom/1, add_cat/2, del_cat/2]).
-export([add_pointer/4, del_pointer/4, get_inv_list/3]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

add_dom(DomId) ->
    dk_ifi_sup:add_dom(DomId).

del_dom(_DomId) ->
    ok.

add_cat(_DomId, _CatId) ->
    ok.

del_cat(_DomId, _CatId) ->
    ok.

add_pointer(_DomId, _CatId, _DocId, _Term) ->
    ok.

del_pointer(_DomId, _CatId, _DocId, _Term) ->
    ok.

get_inv_list(_DomId, _CatId, _Term) ->
    [].

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
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
