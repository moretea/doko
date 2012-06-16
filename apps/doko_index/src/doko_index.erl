-module(doko_index).

%% API
-export([add_doc_id/2, doc_ids/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Term, DocId) ->
    doko_index_term:add_doc_id(doko_index_reg:server(Term), DocId).

doc_ids(Term) ->
    doko_index_term:doc_ids(doko_index_reg:server(Term)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
