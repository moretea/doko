%% @private
-module(dk_meta_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API Functions
%%----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% Supervisor Callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
