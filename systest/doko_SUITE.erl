-module(doko_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Tests
-export([test_queries/1]).

%% CT functions
-export([all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%%----------------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------------

test_queries(_Config) ->
    Nodes = test_nodes(),
    %% add documents
    ok = rpc:call(random(Nodes),
                  doko_ingest, add_doc, [1,<<"hello world">>]),
    ok = rpc:call(random(Nodes),
                  doko_ingest, add_doc, [2,<<"goodbye world">>]),
    ok.

%%----------------------------------------------------------------------------
%% CT functions
%%----------------------------------------------------------------------------

all() ->
    [{group,systest}].

groups() ->
    [{systest,[test_queries]}].

init_per_suite(Config) ->
    Nodes = test_nodes(),
    %% (re)start test nodes
    lists:foreach(fun slave:stop/1, Nodes),
    lists:foreach(
      fun (N) -> {ok,_} = slave:start(host(), short_node_name(N)) end,
      lists:seq(1, length(Nodes))),
    ct_cover:add_nodes(Nodes),
    %% set path on test nodes
    Path = code:get_path(),
    Result = lists:duplicate(length(Nodes), true),
    {Result,[]} = rpc:multicall(Nodes, code, set_path, [Path]),
    %% ready
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Nodes = test_nodes(),
    %% start doko on test nodes
    Result = lists:duplicate(length(Nodes), ok),
    {Result,[]} = rpc:multicall(Nodes, doko_cluster, start, [test_nodes()]),
    {Result,[]} = rpc:multicall(Nodes, doko_node, start, []),
    %% ready
    Config.

end_per_testcase(_TestCase, _Config) ->
    Nodes = test_nodes(),
    %% stop doko on test nodes
    Result = lists:duplicate(length(Nodes), ok),
    {Result,[]} = rpc:multicall(Nodes, doko_cluster, stop, []),
    {Result,[]} = rpc:multicall(Nodes, doko_node, stop, []),
    %% ready
    ok.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

test_nodes() ->
    lists:map(fun (N) -> long_node_name(N) end, lists:seq(1, 5)).

long_node_name(N) ->
    list_to_atom(atom_to_list(short_node_name(N)) ++ "@" ++ host()).

short_node_name(N) ->
    list_to_atom("node" ++ integer_to_list(N)).

host() ->
    lists:takewhile(fun (C) -> C /= $. end, net_adm:localhost()).

random(List) ->
    lists:nth(random:uniform(length(List)), List).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
