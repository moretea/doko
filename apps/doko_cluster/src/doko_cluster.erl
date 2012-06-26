-module(doko_cluster).

%% API
-export([add_index/2,del_index/1,index_lang/1]).
-export([add_doc/3,del_doc/3,doc_ids/2]).
-export([start/1,stop/0]).
-export([where/2]).

-define(RING_SIZE, 420). % number of virtual nodes
-define(N_DUPS, 2). % number of duplicates

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId, Lang) ->
    {ok,Nodes} = application:get_env(doko_cluster, nodes),
    %% TODO: choose appropriate timeout
    Timeout = infinity,
    %% TODO: handle errors
    rpc:multicall(Nodes, doko_node, add_index, [IndexId, Lang], Timeout).

del_index(IndexId) ->
    {ok,Nodes} = application:get_env(doko_cluster, nodes),
    %% TODO: choose appropriate timeout
    Timeout = infinity,
    %% TODO: handle errors
    rpc:multicall(Nodes, doko_node, del_index, [IndexId], Timeout).

index_lang(IndexId) ->
    doko_node:index_lang(IndexId).

%% @doc Adds a document.
add_doc(IndexId, DocId, Terms) ->
    foreach_term(add_doc_id, IndexId, DocId, Terms).

%% @doc Deletes a document.
del_doc(IndexId, DocId, Terms) ->
    foreach_term(del_doc_id, IndexId, DocId, Terms).

doc_ids(IndexId, Term) ->
    get_doc_ids(IndexId, Term).

%% @doc Starts the application.
start(Nodes) ->
    %% TODO: check if number of nodes is at least equal to number of
    %% duplicates
    application:set_env(doko_cluster, nodes, Nodes),
    application:start(doko_cluster).

%% @doc Stops the application.
stop() ->
    application:stop(doko_cluster).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

where(IndexId, Term) ->
    {ok,Nodes} = application:get_env(doko_cluster, nodes),
    Vnode = erlang:phash2({IndexId,Term}, ?RING_SIZE),
    Start = 1 + erlang:trunc((Vnode / ?RING_SIZE) * length(Nodes)),
    lists:sublist(Nodes ++ Nodes, Start, ?N_DUPS).

foreach_term(Fun, IndexId, DocId, Terms) ->
    plists:foreach(
      fun (Term) ->
              %% TODO: choose appropriate timeout
              Timeout = infinity,
              %% TODO: handle errors
              {_,_} = rpc:multicall(where(IndexId, Term),
                                    doko_node, Fun, [IndexId,Term,DocId],
                                    Timeout)
      end,
      Terms).

get_doc_ids(IndexId, Term) ->
    Caller = self(),
    Tag = make_ref(),
    Receiver = doc_ids_receiver(Caller, Tag, IndexId, Term),
    Mref = monitor(process, Receiver),
    Receiver ! {Caller,Tag},
    receive
        {'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
            Result;
        {'DOWN',Mref,_,_,Reason} ->
            %% receiver code failed
            exit(Reason)
    end.

doc_ids_receiver(Caller, Tag, IndexId, Term) ->
    spawn(
      fun () ->
              process_flag(trap_exit, true),
              Mref = monitor(process, Caller),
              receive
                  {'DOWN',Mref,_,_,_} ->
                      %% caller died before sending us the go-ahead
                      exit(normal);
                  {Caller,Tag} ->
                      Keys = lists:map(
                               fun (Node) ->
                                       rpc:async_call(Node,
                                                      doko_node, doc_ids,
                                                      [IndexId,Term])
                               end,
                               where(IndexId, Term)),
                      Result = yield(Keys, 1, length(Keys)),
                      exit({self(),Tag,Result})
              end
      end).

yield(Keys, Index, Length) ->
    Key = lists:nth(Index, Keys),
    case rpc:nb_yield(Key) of
        {value,{badrpc,nodedown}} ->
            yield(lists:delete(Key, Keys), 1, Length - 1);
        {value,Value} ->
            Value;
        timeout ->
            case Index of
                Length -> yield(Keys, 1, Length);
                _      -> yield(Keys, Index + 1, Length)
            end
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
