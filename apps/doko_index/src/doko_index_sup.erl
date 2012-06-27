%% @private
-module(doko_index_sup).

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
    supervisor:start_link({local,name(IndexId)}, ?MODULE, [IndexId]).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([IndexId]) ->
    %% registry supervisor
    RegSup = {doko_index_registry_sup:name(IndexId),
              {doko_index_registry_sup,start_link,[IndexId]},
              permanent,5000,supervisor,[doko_index_registry_sup]},
    %% term supervisor
    TermSup = {doko_index_term_sup:name(IndexId),
               {doko_index_term_sup,start_link,[IndexId]},
               permanent,5000,supervisor,[doko_index_term_sup]},
    {ok,{{one_for_one,5,10},[RegSup,TermSup]}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
