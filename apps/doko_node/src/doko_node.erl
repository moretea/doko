-module(doko_node).

-behaviour(gen_server).

%% API
-export([add_index/2, del_index/1, index_lang/1]).
-export([add_doc_id/4, del_doc_id/4, doc_ids/2]).
-export([start/0, stop/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {table}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId, Lang) ->
    %% register lang
    gen_server:cast(?SERVER, {add, IndexId, Lang}),
    %% start index
    doko_index:add_index(IndexId),
    %% done
    ok.

del_index(IndexId) ->
    %% unregister language
    gen_server:cast(?SERVER, {del, IndexId}),
    %% stop index
    doko_index:del_index(IndexId),
    %% done
    ok.

index_lang(IndexId) ->
    gen_server:call(?SERVER, {get, IndexId}).

add_doc_id(IndexId, Term, DocId, ZoneIds) ->
    doko_index:add_doc_id(IndexId, Term, DocId, ZoneIds).

del_doc_id(IndexId, Term, DocId, ZoneIds) ->
    doko_index:del_doc_id(IndexId, Term, DocId, ZoneIds).

doc_ids(IndexId, Term) ->
    doko_index:doc_ids(IndexId, Term).

%% @doc Starts a node.
start() ->
    application:start(doko_node).

%% @doc Stops the node.
stop() ->
    application:stop(doko_node).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({get, IndexId}, _Client, State) ->
    Reply = case ets:lookup(State#state.table, IndexId) of
                [{IndexId, Lang}] -> Lang;
                [] -> undefined
            end,
    {reply, Reply, State};
handle_call(_Request, _Client, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({add, IndexId, Lang}, State) ->
    ets:insert_new(State#state.table, {IndexId, Lang}),
    {noreply, State};
handle_cast({del, IndexId}, State) ->
    ets:delete(State#state.table, IndexId),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'ETS-TRANSFER', Table, _From, _GiftData}, State) ->
    {noreply, State#state{table = Table}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
