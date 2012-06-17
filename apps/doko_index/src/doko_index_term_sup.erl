%% @private
-module(doko_index_term_sup).

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
    {ok, {{simple_one_for_one, 0, 1},
          [{doko_index_term, {doko_index_term, start_link, []},
            temporary, brutal_kill, worker, [doko_index_term]}]}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
