%% @private
-module(dk_idx_term).

-behaviour(gen_server).

%% API
-export([add_doc_id/2, doc_ids/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Server, DocId) ->
    gen_server:cast(Server, {add, DocId}).

doc_ids(Server) ->
    gen_server:call(Server, get).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, gb_sets:new()}.

%% @private
handle_call(get, _Client, Set = State) ->
    {reply, Set, State};
handle_call(_Request, _Client, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({add, DocId}, Set = _State) ->
    NextState = gb_sets:insert(DocId, Set),
    {noreply, NextState};
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
