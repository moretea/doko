-module(systest).

%% API
-export([run/0]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

run() ->
    ct:run_test([{suite,"doko_SUITE"},{dir,"."},{cover,"cover.spec"},
                 {logdir, "logs"}]).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
