-module(dk_nii_reg).

-behaviour(gen_server).

%% API
-export([name/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

name(N) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ integer_to_list(N) ++ "]").

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
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
    {ok, State}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
