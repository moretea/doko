-module(doko_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Tests
-export([test_del_doc/1,test_del_index/1,test_queries/1,test_redundancy/1,
         test_replication/1]).

%% CT functions
-export([all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%%----------------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------------

test_queries(_Config) ->
    Nodes = test_nodes(),
    %% add index
    Index = index,
    rpc:call(random(Nodes), doko_cluster, add_index, [Index, "en"]),
    %% add documents
    ok = rpc:call(random(Nodes),
                  doko_ingest, add_doc, [Index,1,<<"hello world">>]),
    ok = rpc:call(random(Nodes),
                  doko_ingest, add_doc, [Index,2,<<"goodbye world">>]),
    ok = rpc:call(random(Nodes),
                  doko_ingest, add_doc, [Index,3,<<"aloha world">>]),
    %% test queries
    Result1 = rpc:call(random(Nodes), doko_query, execute,
                       [Index,<<"aloha">>]),
    timer:sleep(1000),
    [3] = gb_sets:to_list(Result1),
    Result2 = rpc:call(random(Nodes), doko_query, execute,
                       [Index,<<"(hello | goodbye) & world & !aloha">>]),
    [1,2] = lists:sort(gb_sets:to_list(Result2)),
    ok.

test_replication(_Config) ->
    Nodes = test_nodes(),
    %% add index
    Index = index,
    rpc:call(random(Nodes), doko_cluster, add_index, [Index, "en"]),
    %% add document
    ok = rpc:call(random(Nodes), doko_ingest, add_doc,
                  [Index,1,<<"hello world">>]),
    %% test replication
    {Result,[]} = rpc:multicall(Nodes, doko_index, doc_ids,
                                [Index,<<"hello">>]),
    3 = length(lists:filter(fun gb_sets:is_empty/1, Result)),
    ok.

test_del_doc(_Config) ->
    Nodes = test_nodes(),
    %% add index
    Index = index,
    rpc:call(random(Nodes), doko_cluster, add_index, [Index, "en"]),
    %% add document
    ok = rpc:call(random(Nodes), doko_ingest, add_doc,
                  [Index,1,<<"hello world">>]),
    %% execute query and check result
    Query = fun () ->
                    rpc:call(random(Nodes),
                             doko_query, execute, [Index,<<"hello">>])
            end,
    [1] = gb_sets:to_list(Query()),
    %% delete document
    ok = rpc:call(random(Nodes), doko_ingest, del_doc,
                  [Index,1,<<"hello world">>]),
    %% execute query and check result
    Result2 = rpc:call(random(Nodes), doko_query, execute,
                       [Index,<<"hello">>]),
    true = gb_sets:is_empty(Result2),
    ok.

test_redundancy(Config) ->
    Nodes = test_nodes(),
    %% add index
    Index = index,
    rpc:call(random(Nodes), doko_cluster, add_index, [Index, "en"]),
    %% add document
    ok = rpc:call(random(Nodes), doko_ingest, add_doc,
                  [Index,1,<<"hello world">>]),
    %% stop one of the nodes that has the data
    [Node|_] = rpc:call(random(Nodes), doko_cluster, where,
                        [Index,<<"hello">>]),
    slave:stop(Node),
    %% execute query and check result
    Result = rpc:call(random(lists:delete(Node,Nodes)),
                      doko_query, execute, [Index,<<"hello">>]),
    [1] = gb_sets:to_list(Result),
    %% restart node
    start_node(Node,Config),
    ok.

test_del_index(_Config) ->
    Nodes = test_nodes(),
    %% add index
    Index = index,
    rpc:call(random(Nodes), doko_cluster, add_index, [Index, "en"]),
    Name = doko_index_sup:name(Index),
    {Result1,[]} = rpc:multicall(Nodes, erlang, whereis, [Name]),
    0 = length([X||X <- Result1, X == undefined]),
    %% del index
    rpc:call(random(Nodes), doko_cluster, del_index, [Index]),
    {Result2,[]} = rpc:multicall(Nodes, erlang, whereis, [Name]),
    Length = length(Nodes),
    Length = length([X||X <- Result2, X == undefined]),
    %% done
    ok.

%%----------------------------------------------------------------------------
%% CT functions
%%----------------------------------------------------------------------------

all() ->
    [{group,systest}].

groups() ->
    [{systest,[shuffle,sequence,{repeat,10}],[test_queries,
                                              test_replication,
                                              test_del_doc,
                                              test_redundancy,
                                              test_del_index]}].

init_per_suite(Config) ->
    Nodes = test_nodes(),
    %% put path in config
    Path = code:get_path(),
    NewConfig = [{path,Path}|Config],
    %% (re)start test nodes
    lists:foreach(fun (Node) -> start_node(Node, NewConfig) end, Nodes),
    ct_cover:add_nodes(Nodes),
    %% ready
    NewConfig.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Nodes = test_nodes(),
    %% start doko on test nodes
    Result = lists:duplicate(length(Nodes), ok),
    {Result,[]} = rpc:multicall(Nodes, doko_node, start, []),
    {Result,[]} = rpc:multicall(Nodes, doko_cluster, start, [Nodes]),
    %% ready
    Config.

end_per_testcase(_TestCase, Config) ->
    case ?config(tc_status, Config) of
        {failed,_} ->
            %% leave doko running for inspection
            ok;
        ok ->
            Nodes = test_nodes(),
            %% stop doko on test nodes
            rpc:multicall(Nodes, doko_cluster, stop, []),
            rpc:multicall(Nodes, doko_node, stop, []),
            %% ready
            ok
    end.

start_node(Node, Config) ->
    slave:stop(Node),
    {ok,_} = slave:start(host(), short_name(Node)),
    Path = ?config(path, Config),
    rpc:call(Node, code, set_path, [Path]).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

test_nodes() ->
    lists:map(fun (N) -> node_name(N) end, lists:seq(1, 5)).

node_name(N) ->
    list_to_atom("doko_systest_node" ++ integer_to_list(N) ++ "@" ++ host()).

short_name(Node) ->
    list_to_atom(lists:takewhile(fun (C) -> C/= $@ end, atom_to_list(Node))).

host() ->
    lists:takewhile(fun (C) -> C /= $. end, net_adm:localhost()).

random(List) ->
    lists:nth(random:uniform(length(List)), List).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
