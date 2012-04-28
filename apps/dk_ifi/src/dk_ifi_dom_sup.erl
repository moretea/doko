-module(dk_ifi_dom_sup).

-behaviour(supervisor).

%% API
-export([name/1]).
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%% API 

name(DomId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ DomId ++ "]").

start_link(DomId) ->
    supervisor:start_link({local, name(DomId)}, ?MODULE, []).

%%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
