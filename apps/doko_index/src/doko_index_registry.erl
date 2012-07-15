%% @private
-module(doko_index_registry).

-behaviour(gen_server).

%% API
-export([server/2,server/3]).
-export([name/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

server(IndexId, Term) ->
    gen_server:call(name(IndexId), {server, IndexId, Term, false}).

server(IndexId, Term, create) ->
    gen_server:call(name(IndexId), {server, IndexId, Term, true}).

name(IndexId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ IndexId ++ "]").

start_link(IndexId) ->
    gen_server:start_link({local, name(IndexId)}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, Dict} = sdict:start_link(),
    State = Dict,
    {ok, State}.

%% @private
handle_call({server,IndexId,Term,Create}, _From, Dict = State) ->
    Reply =
        case {sdict:find(Term, Dict),Create} of
            {{ok,Value},_} ->
                Value;
            {error,false} ->
                undefined;
            {error,true} ->
                SupRef = doko_index_term_sup:name(IndexId),
                {ok, Server} = supervisor:start_child(SupRef, []),
                sdict:store(Term, Server, Dict),
                Server
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
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
    {ok,State}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
