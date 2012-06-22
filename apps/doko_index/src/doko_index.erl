-module(doko_index).

%% API
-export([add_index/1]).
-export([add_doc_id/2,del_doc_id/2,doc_ids/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId) ->
    doko_index_top_sup:add_index(IndexId).
    
add_doc_id(Term, DocId) ->
    doko_index_term:add_doc_id(doko_index_registry:server(Term, create),
                               DocId).

del_doc_id(Term, DocId) ->
    doko_index_term:del_doc_id(doko_index_registry:server(Term), DocId).

doc_ids(Term) ->
    case doko_index_registry:server(Term) of
        undefined -> gb_sets:empty();
        Server    -> doko_index_term:doc_ids(Server)
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
