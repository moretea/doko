-module(dk_ifi_app).
-behaviour(application).
-export([start/2, stop/1]).

%%% Application callbacks

start(_StartType, _StartArgs) ->
    dk_ifi_sup:start_link().

stop(_State) ->
    ok.

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
