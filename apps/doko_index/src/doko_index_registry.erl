%% @private
-module(doko_index_registry).
-include("doko_index.hrl").

-behaviour(gen_server).

%% API
-export([server/2,server/3]).
-export([name/2]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

server(IndexId, Term) ->
    gen_server:call(name(IndexId, erlang:phash2(Term, ?SIZE)),
                    {server,IndexId,Term,false}).

server(IndexId, Term, create) ->
    gen_server:call(name(IndexId, erlang:phash2(Term, ?SIZE)),
                    {server,IndexId,Term,true}).

name(IndexId, N) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ atom_to_list(IndexId) ++ "]["
                 ++ integer_to_list(N) ++ "]").

start_link(Name) ->
    gen_server:start_link({local,Name}, ?MODULE, [], []).


%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok,dict:new()}.

%% @private
handle_call({server,IndexId,Term,Create}, _From, Dict = State) ->
    {Server,NextState} =
        case {dict:find(Term, Dict),Create} of
            {{ok,Value},_} ->
                {Value,State};
            {error,false} ->
                {undefined,State};
            {error,true} ->
                SupRef = doko_index_term_sup:name(IndexId),
                {ok,NewServer} =
                    supervisor:start_child(SupRef, []),
                {NewServer,dict:store(Term, NewServer, Dict)}
        end,
    {reply,Server,NextState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply,Reply,State}.

%% @private
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
