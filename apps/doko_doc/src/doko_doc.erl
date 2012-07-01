-module(doko_doc).
-include("../include/doko_doc.hrl").

%% API
-export([new/3]).
-export([index_id/1,doc_id/1,uterms/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec new(index_id(), doc_id(), nonempty_list(zone())) -> #doc{}.
new(IndexId, DocId, Zones) ->
    Lang = doko_cluster:index_lang(IndexId),
    Preprocess = fun (Text) -> doko_preprocessing:uterms(Text, Lang) end,
    #doc{index_id = IndexId,
         doc_id = DocId,
         zones = [{ZoneId,Preprocess(Text)} || {ZoneId,Text} <- Zones]}.

-spec index_id(#doc{}) -> index_id().
index_id(#doc{index_id = IndexId}) ->
    IndexId.

-spec doc_id(#doc{}) -> doc_id().
doc_id(#doc{doc_id = DocId}) ->
    DocId.

-spec uterms(#doc{}) -> list(utf8_string()).
uterms(#doc{zones = Zones}) ->
    lists:usort(lists:flatmap(fun ({_Id,Terms}) -> Terms end, Zones)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
