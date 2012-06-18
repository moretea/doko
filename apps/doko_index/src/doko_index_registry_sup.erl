%% @private
-module(doko_index_registry_sup).
-include("doko_index.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [child_spec(N)||N <- lists:seq(0, ?SIZE - 1)]}}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

child_spec(N) ->
    Name = doko_index_registry:name(N),
    Mod = doko_index_registry,
    {Name,{Mod,start_link,[Name]},permanent,2000,worker,[Mod]}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
