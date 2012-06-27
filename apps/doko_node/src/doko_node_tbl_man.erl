-module(doko_node_tbl_man).

-export([init/0,start_link/0]).

-record(state, {table}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

init() ->
    %% link to table user
    true = link(whereis(doko_node)),
    %% trap exits
    process_flag(trap_exit, true),
    %% create table
    %% give table away
    %% enter main loop
    loop(#state{}).

loop(State) ->
    receive
        _ ->
            loop(State)
    end.

start_link() ->
    Pid = spawn(?MODULE, init, []),
    {ok,Pid}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
