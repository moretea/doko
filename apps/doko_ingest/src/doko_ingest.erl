-module(doko_ingest).

%% API
-export([add_index/2]).
-export([add_doc/3,del_doc/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId, Lang) ->
    doko_cluster:add_index(IndexId, Lang).

add_doc(IndexId, DocId, Text) ->
    %% FIXME: hardcoded language
    doko_cluster:add_doc(IndexId, DocId,
                         doko_preprocessing:uterms(Text, "en")).

del_doc(IndexId, DocId, Text) ->
    %% FIXME: hardcoded language
    doko_cluster:del_doc(IndexId, DocId,
                         doko_preprocessing:uterms(Text, "en")).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
