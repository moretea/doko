-module(doko_cluster).

%% API
-export([add_doc/2,del_doc/2,doc_ids/1]).
-export([start/1,stop/0]).
-export([where/1]).

-define(RING_SIZE, 420). % number of virtual nodes
-define(N_DUPS, 2). % number of duplicates

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Adds a document.
add_doc(DocId, Terms) ->
    foreach_term(add_doc_id, DocId, Terms).

%% @doc Deletes a document.
del_doc(DocId, Terms) ->
    foreach_term(del_doc_id, DocId, Terms).

doc_ids(Term) ->
    get_doc_ids(Term).

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

where(Term) ->
    {ok, Nodes} = application:get_env(doko_cluster, nodes),
    Vnode = erlang:phash2(Term, ?RING_SIZE),
    Start = 1 + erlang:trunc((Vnode / ?RING_SIZE) * length(Nodes)),
    lists:sublist(Nodes ++ Nodes, Start, ?N_DUPS).

foreach_term(Fun, DocId, Terms) ->
    plists:foreach(
      fun (Term) ->
              %% TODO: choose appropriate timeout
              Timeout = infinity,
              %% TODO: handle errors
              {_,_} = rpc:multicall(where(Term),
                                    doko_node, Fun, [Term, DocId], Timeout)
      end,
      Terms).

get_doc_ids(Term) ->
    Caller = self(),
    Tag = make_ref(),
    Receiver = doc_ids_receiver(Caller, Tag, Term),
    Mref = monitor(process, Receiver),
    Receiver ! {Caller,Tag},
    receive
        {'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
            Result;
        {'DOWN',Mref,_,_,Reason} ->
            %% receiver code failed
            exit(Reason)
    end.

doc_ids_receiver(Caller, Tag, Term) ->
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
                                                      [Term])
                               end,
                               where(Term)),
                      Result = yield(Keys, 1, length(Keys)),
                      exit({self(),Tag,Result})
              end
      end).

yield(Keys, Index, Length) ->
    case rpc:nb_yield(lists:nth(Index, Keys)) of
        {value,Value} -> Value;
        timeout       -> case Index of
                             Length -> yield(Keys, 1, Length);
                             _      -> yield(Keys, Index + 1, Length)
                         end
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
