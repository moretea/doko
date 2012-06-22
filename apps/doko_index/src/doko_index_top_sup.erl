%% @private
-module(doko_index_top_sup).

-behaviour(supervisor).

%% API
-export([add_index/1]).
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_index(IndexId) ->
    supervisor:start_child(?MODULE, [IndexId]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok,{{simple_one_for_one,0,1},
         [{doko_index_sup,{doko_index_sup,start_link,[]},
           temporary,brutal_kill,worker,[doko_index,sup]}]}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
