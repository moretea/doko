%% @private
-module(dk_ii_cat).

-behavior(gen_server).

%% API
-export([name/2]).
-export([add_post/4, del_post/4, get_posts/3]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {table}).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

name(DomId, CatId) ->
    list_to_atom(?MODULE_STRING ++ "[" ++ DomId ++ "][" ++ CatId ++ "]").

add_post(DomId, CatId, DocId, Term) ->
    gen_server:cast(name(DomId, CatId), {add, Term, DocId}).

del_post(DomId, CatId, DocId, Term) ->
    gen_server:cast(name(DomId, CatId), {del, Term, DocId}).

get_posts(DomId, CatId, Term) ->
    case ets:lookup(tid(DomId, CatId), Term) of
        [] -> sets:new();
        [{Term, Set}] -> Set
    end.

start_link(DomId, CatId) ->
    gen_server:start_link(
      {local, name(DomId, CatId)}, ?MODULE, [DomId, CatId], []).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

init([DomId, CatId]) ->
    Table =ets:new(tid(DomId, CatId), [set, public, named_table,
                                       {write_concurrency, false},
                                       {read_concurrency, true}]),
    {ok, #state{table = Table}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add, Term, DocId}, State) ->
    Table = State#state.table,
    ets:insert(Table,
               {Term, sets:add_element(DocId,
                                       case ets:lookup(Table, Term) of
                                           [] -> sets:new();
                                           [{Term, Set}] -> Set
                                       end)}),
    {noreply, State};
handle_cast({del, Term, DocId}, State) ->
    Table = State#state.table,
    case ets:lookup(Table, Term) of
        [] -> ok;
        [{Term, Set}] -> ets:insert(Table,
                                    {Term, sets:del_element(DocId, Set)})
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------

tid(DomId, CatId) ->
    list_to_atom("dk_ii[" ++ DomId ++ "][" ++ CatId ++ "]").

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
