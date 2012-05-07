%% @private
-module(dk_ii_sup).

-behaviour(supervisor).

%% API
-export([add_dom/1, del_dom/1]).
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_dom(DomId) ->
    supervisor:start_child(
      ?MODULE, {dk_ii_dom_sup:name(DomId),
                {dk_ii_dom_sup, start_link, [DomId]},
                transient, 5000, supervisor, [dk_ii_dom_sup]}).

del_dom(DomId) ->
    Name = dk_ii_dom_sup:name(DomId),
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
