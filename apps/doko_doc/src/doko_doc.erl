-module(doko_doc).

%% API
-export([new/2]).
-export([id/1,zone/2]).

-record(doc, {id, zones}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

new(Id, Zones) ->
    #doc{id = Id,zones = dict:from_list(Zones)}.

id(#doc{id = Id}) ->
    Id.

zone(ZoneName, #doc{zones = Zones}) ->
    case dict:find(ZoneName, Zones) of
        {ok,Value} -> Value;
        error -> undefined
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
