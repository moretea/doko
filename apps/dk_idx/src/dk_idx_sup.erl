%% @private
-module(dk_idx_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 2000, Type, [I]}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(dk_idx_reg_sup, supervisor),
                                 ?CHILD(dk_idx_term_sup, supervisor)]}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End: