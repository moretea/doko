-module(dk_ifi_dom_sup).

-behaviour(supervisor).

%% API
-export([name/1]).
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%% API 

name(DomId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ DomId ++ "]").

start_link(DomId) ->
    supervisor:start_link({local, name(DomId)}, ?MODULE, []).

%%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
