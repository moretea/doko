-module(doko_index).

%% API
-export([add_index/1,del_index/1]).
-export([add_doc_id/3,del_doc_id/3,doc_ids/2]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId) ->
    doko_index_top_sup:add_index(IndexId).

del_index(IndexId) ->
    doko_index_top_sup:del_index(IndexId).
    
add_doc_id(IndexId, Term, DocId) ->
    doko_index_term:add_doc_id(
      doko_index_registry:server(IndexId, Term, create),
      DocId).

del_doc_id(IndexId, Term, DocId) ->
    doko_index_term:del_doc_id(
      doko_index_registry:server(IndexId, Term),
      DocId).

doc_ids(IndexId, Term) ->
    case doko_index_registry:server(IndexId, Term) of
        undefined -> gb_sets:empty();
        Server    -> doko_index_term:doc_ids(Server)
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
