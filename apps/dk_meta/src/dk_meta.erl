-module(dk_meta).

-behaviour(gen_server).

%% API
-export([add_dom/2, del_dom/1, dom_lang/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {dom_lang}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Adds a domain.
add_dom(DomId, Lang) ->
    gen_server:call(?SERVER, {add_dom, DomId, Lang}).
    
%% @doc Adds a domain.
del_dom(DomId) ->
    gen_server:call(?SERVER, {del_dom, DomId}).
    
%% @doc Returns the language of a domain.
dom_lang(DomId) ->
    gen_server:call(?SERVER, {dom_lang, DomId}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{dom_lang = dict:new()}}.

%% @private
handle_call({add_dom, DomId, Lang}, _From, State) ->
    Dict = State#state.dom_lang,
    case dict:is_key(DomId, Dict) of
        true ->
            {reply, error, State};
        false ->
            NextState = State#state{dom_lang = dict:store(DomId, Lang, Dict)},
            {reply, ok, NextState}
    end;
handle_call({del_dom, DomId}, _From, State) ->
    NextState =
        State#state{dom_lang = dict:erase(DomId, State#state.dom_lang)},
    {reply, ok, NextState};
handle_call({dom_lang, DomId}, _From, State) ->
    Reply = dict:find(DomId, State#state.dom_lang),
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
