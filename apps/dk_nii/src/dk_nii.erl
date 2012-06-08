-module(dk_nii).

%% API
-export([add_doc_id/2, doc_ids/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Term, DocId) ->
    dk_nii_term:add_doc_id(dk_nii_reg:server(Term), DocId).

doc_ids(Term) ->
    dk_nii_term:doc_ids(dk_nii_reg:server(Term)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
