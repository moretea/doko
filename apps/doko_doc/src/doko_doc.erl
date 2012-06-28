-module(doko_doc).
-include("../include/doko_doc.hrl").

%% API
-export([new/2,preprocess/2]).
-export([id/1,uterms/1]).

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

-spec uterms(#doc{}) -> list(utf8_string()).
uterms(#doc{zones = Zones,raw = false}) ->
    lists:usort(lists:flatmap(fun ({_Id,Terms}) -> Terms end, Zones)).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
