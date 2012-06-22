-module(doko_node).

-behaviour(gen_server).

%% API
-export([add_index/2]).
-export([add_doc_id/3,del_doc_id/3,doc_ids/2]).
-export([start/0,stop/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId, Lang) ->
    %% register lang
    gen_server:cast(?SERVER, {add,IndexId,Lang}),
    %% start index
    doko_index:add_index(IndexId),
    %% done
    ok.

add_doc_id(IndexId, Term, DocId) ->
    doko_index:add_doc_id(IndexId, Term, DocId).

del_doc_id(IndexId, Term, DocId) ->
    doko_index:del_doc_id(IndexId, Term, DocId).

doc_ids(IndexId, Term) ->
    doko_index:doc_ids(IndexId, Term).

%% @doc Starts a node.
start() ->
    application:start(doko_node).

%% @doc Stops the node.
stop() ->
    application:stop(doko_node).

start_link() ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok,dict:new()}.

%% @private
handle_call(_Request, _Client, State) ->
    Reply = ok,
    {reply,Reply,State}.

%% @private
handle_cast({add,IndexId,Lang}, Dict = State) ->
    NextState = case dict:is_key(IndexId, Dict) of
                    true  -> State;
                    false -> dict:store(IndexId, Lang, Dict)
                end,
    {noreply,NextState};
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
handle_info(_Info, State) ->
    {noreply,State}.

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
