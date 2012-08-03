-module(doko_node_tbl_man).

-export([init/0,start_link/0]).

-define(PAUSE, 10).
-define(USER, doko_node).

-record(state, {table}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

init() ->
    Table = ets:new(index_conf, [set,protected,{heir,self(),[]}]),
    reset(Table).

start_link() ->
    Pid = spawn(?MODULE, init, []),
    {ok,Pid}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

reset(Table) ->
    case whereis(?USER) of
        undefined ->
            timer:sleep(?PAUSE),
            reset(Table);
        TableUser ->
            %% link to table user
            true = link(TableUser),
            %% trap exits
            process_flag(trap_exit, true),
            %% give table away
            ets:give_away(Table, TableUser, []),
            %% enter loop
            loop(#state{})
    end.

loop(State) ->
    receive
        {'ETS-TRANSFER',Table,_,_} ->
            loop(State#state{table = Table});
        {'EXIT',_,killed} ->
            reset(State#state.table);
        {'EXIT',_,shutdown} ->
            unlink(whereis(?USER)),
            exit(shutdown);
        _ ->
            loop(State)
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
