-module(doko_ingest).

%% API
-export([add_doc/3,del_doc/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc(IndexId, DocId, Text) ->
    Lang = doko_cluster:index_lang(IndexId),
    doko_cluster:add_doc(IndexId, DocId,
                         doko_preprocessing:uterms(Text, Lang)).

del_doc(IndexId, DocId, Text) ->
    Lang = doko_cluster:index_lang(IndexId),
    doko_cluster:del_doc(IndexId, DocId,
                         doko_preprocessing:uterms(Text, Lang)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
