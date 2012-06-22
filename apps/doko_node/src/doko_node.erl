-module(doko_node).

%% API
-export([add_doc_id/2,del_doc_id/2,doc_ids/1]).
-export([start/0,stop/0]).
%% -export([load/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Term, DocId) ->
    doko_index:add_doc_id(Term, DocId).

del_doc_id(Term, DocId) ->
    doko_index:del_doc_id(Term, DocId).

doc_ids(Term) ->
    doko_index:doc_ids(Term).

%% @doc Starts a node.
start() ->
    application:start(doko_node).

%% @doc Stops the node.
stop() ->
    application:stop(doko_node).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
