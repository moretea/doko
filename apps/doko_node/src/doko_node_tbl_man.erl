-module(doko_node_tbl_man).

-export([loop/0,start_link/0]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

loop() ->
    receive
        init ->
            true = link(whereis(doko_node)),
            loop();
        _ ->
            loop()
    end.

start_link() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! init,
    {ok,Pid}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
