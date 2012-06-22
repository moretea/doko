-module(doko_ingest).

%% API
-export([add_doc/2,del_doc/2]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc(DocId, Text) ->
    %% FIXME: hardcoded language
    doko_cluster:add_doc(DocId, doko_preprocessing:uterms(Text, "en")).

del_doc(DocId, Text) ->
    %% FIXME: hardcoded language
    doko_cluster:del_doc(DocId, doko_preprocessing:uterms(Text, "en")).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
