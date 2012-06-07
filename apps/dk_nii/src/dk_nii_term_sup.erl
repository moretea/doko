%% @private
-module(dk_nii_term_sup).

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
          [{dk_nii_term, {dk_nii_term, start_link, []},
            temporary, brutal_kill, worker, [dk_nii_term]}]}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
