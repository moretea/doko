-module(dk_idx).

%% API
-export([add_doc_id/2, doc_ids/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Term, DocId) ->
    dk_idx_term:add_doc_id(dk_idx_reg:server(Term), DocId).

doc_ids(Term) ->
    dk_idx_term:doc_ids(dk_idx_reg:server(Term)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
