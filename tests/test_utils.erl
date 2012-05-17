-module(test_utils).

%% API
-export([run_suites/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

run_suites(Suites) ->
    lists:foreach(
      fun (Suite) ->
              Cover = Suite ++ ".spec",
              Opts = [{suite, Suite}, {dir, "."}, {cover, Cover},
                      {logdir, "logs"}],
              ct:run_test(Opts)
      end,
      [atom_to_list(Suite) ++ "_SUITE" || Suite <- Suites]).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
