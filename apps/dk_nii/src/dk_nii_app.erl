%% @private
-module(dk_nii_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%----------------------------------------------------------------------------
%% application callbacks
%%----------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    dk_nii_sup:start_link().

stop(_State) ->
    ok.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
