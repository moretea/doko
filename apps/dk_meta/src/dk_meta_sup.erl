%% @private
-module(dk_meta_sup).

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
    {ok, {{one_for_one, 5, 10}, [{dk_meta, {dk_meta, start_link, []},
                                  permanent, 2000, worker, [dk_meta]}]} }.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
