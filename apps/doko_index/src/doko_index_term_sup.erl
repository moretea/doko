%% @private
-module(doko_index_term_sup).

-behaviour(supervisor).

%% API
-export([name/1]).
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

name(IndexId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ IndexId ++ "]").

start_link(IndexId) ->
    supervisor:start_link({local, name(IndexId)}, ?MODULE, []).

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
