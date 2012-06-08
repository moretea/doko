%% @private
-module(dk_idx_reg_sup).
-include("dk_idx.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [child_spec(N) || N <- lists:seq(0, ?SIZE - 1)]}}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

child_spec(N) ->
    Name = dk_idx_reg:name(N),
    Mod = dk_idx_reg,
    {Name, {Mod, start_link, [Name]}, permanent, 2000, worker, [Mod]}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
