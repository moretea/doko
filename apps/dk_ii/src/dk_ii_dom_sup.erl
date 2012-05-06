%% @private
-module(dk_ii_dom_sup).

-behaviour(supervisor).

%% API
-export([name/1, add_cat/2, del_cat/2]).
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%----------------------------------------------------------------------------
%% API 
%%----------------------------------------------------------------------------

name(DomId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ DomId ++ "]").

add_cat(DomId, CatId) ->
    supervisor:start_child(
      name(DomId), {dk_ii_cat:name(DomId, CatId),
                    {dk_ii_cat, start_link, [DomId, CatId]},
                    transient, 5000, worker, [dk_ii_cat]}).

del_cat(DomId, CatId) ->
    SupRef = name(DomId),
    Name = dk_ii_cat:name(DomId, CatId),
    supervisor:terminate_child(SupRef, Name),
    supervisor:delete_child(SupRef, Name).

start_link(DomId) ->
    supervisor:start_link({local, name(DomId)}, ?MODULE, []).

%%----------------------------------------------------------------------------
%% supervisor callbacks
%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
