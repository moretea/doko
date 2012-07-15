%% @private
-module(doko_index_term).

-behaviour(gen_server).

%% API
-export([add_doc_id/3, del_doc_id/3, doc_ids/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Server, DocId, ZoneIds) ->
    gen_server:cast(Server, {add, DocId, [any | ZoneIds]}).

del_doc_id(Server, DocId, ZoneIds) ->
    case Server of
        undefined -> ok;
        _ -> gen_server:cast(Server, {del, DocId, [any | ZoneIds]})
    end.

doc_ids(Server) ->
    gen_server:call(Server, get).

start_link() ->
    gen_server:start_link(?MODULE,[],[]).

%%----------------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------------

%% @private
init([]) ->
    Dict = dict:from_list([{any, gb_sets:empty()}]),
    {ok, Dict}.

%% @private
handle_call(get, _Client, Dict = State) ->
    {reply,Dict,State};
handle_call(_Request, _Client, State) ->
    Reply = ok,
    {reply,Reply,State}.

%% @private
handle_cast({add, DocId, ZoneIds}, Dict0 = _State) ->
    Fun = fun (ZoneId, Dict) ->
                  Set = case dict:find(ZoneId, Dict) of
                            {ok, OldSet} ->
                                gb_sets:add_element(DocId, OldSet);
                            error ->
                                gb_sets:from_list([DocId])
                        end,
                  dict:store(ZoneId, Set, Dict)
          end,
    NextState = lists:foldl(Fun, Dict0, ZoneIds),
    {noreply,NextState};
handle_cast({del, DocId, ZoneIds}, Dict0 = _State) ->
    Fun = fun (ZoneId, Dict) ->
                  Set = case dict:find(ZoneId, Dict) of
                            {ok, OldSet} ->
                                gb_sets:del_element(DocId, OldSet);
                            error ->
                                gb_sets:empty()
                        end,
                  dict:store(ZoneId, Set, Dict)
          end,
    NextState = lists:foldl(Fun, Dict0, ZoneIds),
    {noreply,NextState};
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
handle_info(_Info, State) ->
    {noreply,State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
