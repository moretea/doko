-module(doko_doc).
-include("../../doko_utf8/include/doko_utf8.hrl").

%% API
-export([new/3]).
-export([index_id/1, doc_id/1, terms_x_zones/1]).
-export_type([doc/0, doc_id/0, index_id/0, zone_id/0]).

%% Type declarations
-type index_id() :: nonempty_string().
-type doc_id() :: pos_integer().
-type zone_id() :: nonempty_string().
-type zone() :: {zone_id(), utf8_string()}.

%% Record definitions
-record(doc, {index_id :: index_id(),
              doc_id :: doc_id(),
              zones :: dict()}).
-opaque doc() :: #doc{}.

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec new(index_id(), doc_id(), [zone(), ...]) -> doc().
new(IndexId, DocId, Zones) ->
    Lang = doko_cluster:index_lang(IndexId),
    Preprocess = fun (Text) -> doko_preprocessing:uterms(Text, Lang) end,
    List = [{ZoneId, Preprocess(Text)} || {ZoneId, Text} <- Zones],
    #doc{index_id = IndexId, doc_id = DocId, zones = dict:from_list(List)}.

-spec index_id(doc()) -> index_id().
index_id(#doc{index_id = IndexId}) ->
    IndexId.

-spec doc_id(doc()) -> doc_id().
doc_id(#doc{doc_id = DocId}) ->
    DocId.

-spec terms_x_zones(doc()) -> [{utf8_string(), [zone_id(), ...]}].
terms_x_zones(#doc{zones = Zones}) ->
    dict:to_list(
      lists:foldl(
        fun ({Term, ZoneId}, Dict) -> dict:append(Term, ZoneId, Dict) end,
        dict:new(),
        lists:flatmap(
          fun (ZoneId) ->
                  [{Term, ZoneId} || Term <- dict:fetch(ZoneId, Zones)]
          end,
          dict:fetch_keys(Zones)))).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
