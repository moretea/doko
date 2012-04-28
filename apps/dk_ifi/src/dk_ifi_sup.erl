-module(dk_ifi_sup).

-behaviour(supervisor).

%% API
-export([add_dom/1, del_dom/1]).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%% API

add_dom(DomId) ->
    supervisor:start_child(
      ?MODULE, {dk_ifi_dom_sup:name(DomId),
                {dk_ifi_dom_sup, start_link, [DomId]},
                transient, 5000, supervisor, [dk_ifi_dom_sup]}).

del_dom(DomId) ->
    Name = dk_ifi_dom_sup:name(DomId),
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(dk_ifi, worker)]}}.

