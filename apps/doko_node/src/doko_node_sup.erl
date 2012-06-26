%% @private
-module(doko_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),{I,{I,start_link,[]},permanent,5000,Type,[I]}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok,{{one_for_one,5,10},[?CHILD(doko_node, worker),
                             ?CHILD(doko_index_top_sup, supervisor),
                             ?CHILD(doko_node_tbl_man, worker)]}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
