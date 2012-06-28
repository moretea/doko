-module(doko_doc).
-include("../include/doko_doc.hrl").

%% API
-export([new/2,preprocess/2]).
-export([id/1,zone/2]).
-export([zone_ids/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec new(doc_id(), nonempty_list(zone())) -> #doc{}.
new(Id, Zones) ->
    #doc{id = Id,zones = Zones}.

-spec preprocess(#doc{}, iso_639_1()) -> #doc{}.
preprocess(#doc{id = Id,zones = RawZones,raw = true}, Lang) ->
    Preprocess = fun (Text) -> doko_preprocessing:uterms(Text, Lang) end,
    #doc{id    = Id,
         zones = [{ZoneId,Preprocess(Text)} || {ZoneId,Text} <- RawZones],
         raw   = false}.

-spec id(#doc{}) -> doc_id().
id(#doc{id = Id}) ->
    Id.

-spec zone(zone_id(), #doc{}) -> utf8_string() | undefined.
zone(ZoneId, #doc{zones = Zones}) ->
    proplists:get_value(ZoneId, Zones, undefined).

-spec zone_ids(#doc{}) -> nonempty_list(zone_id()).
zone_ids(#doc{zones = Zones}) ->
    proplists:get_keys(Zones).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
