-module(doko_doc).

%% API
-export([new/3]).
-export([doc_id/1, terms_x_zones/1]).
-export_type([doc/0, doc_id/0, zone_id/0]).

%% Type declarations
-type doc_id() :: pos_integer().
-type zone_id() :: nonempty_string().
-type zone() :: {zone_id(), doko_utf8:str()}.

%% Record definitions
-record(doc, {doc_id :: doc_id(), zones :: dict()}).
-opaque doc() :: #doc{}.

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec new(doc_id(), [zone(), ...], doko_utf8:iso_639_1()) -> doc().
new(DocId, Zones, Lang) ->
    Preprocess = fun (Text) -> doko_preprocessing:uterms(Text, Lang) end,
    List = [{ZoneId, Preprocess(Text)} || {ZoneId, Text} <- Zones],
    #doc{doc_id = DocId, zones = dict:from_list(List)}.

-spec doc_id(doc()) -> doc_id().
doc_id(#doc{doc_id = DocId}) ->
    DocId.

-spec terms_x_zones(doc()) -> [{doko_utf8:str(), [zone_id(), ...]}].
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
