%% @private
-module(doko_index_registry_sup).
-include("doko_index.hrl").

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
    list_to_atom(?MODULE_STRING ++ "[" ++ atom_to_list(IndexId) ++ "]").

start_link(IndexId) ->
    supervisor:start_link({local,name(IndexId)}, ?MODULE, [IndexId]).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([IndexId]) ->
    {ok, {{one_for_one, 5, 10},
          [child_spec(IndexId, N)||N <- lists:seq(0, ?SIZE - 1)]}}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

child_spec(IndexId, N) ->
    Name = doko_index_registry:name(IndexId, N),
    Mod = doko_index_registry,
    {Name,{Mod,start_link,[Name]},permanent,2000,worker,[Mod]}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
