-module(doko_benchmark).

%% API
-export([load_file/4, time_query/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

load_file(Path, DomId, Lang, CatId) ->
    try
        doko:del_dom(DomId)
    catch
        _:_ -> whatever
    end,
    doko:add_dom(DomId, Lang),
    doko:add_cat(DomId, CatId),
    {ok, Io} = file:open(Path, [read]),
    read_line(Io, DomId, Lang, CatId).

time_query(DomId, CatId, Query) ->
    {Time, _} = timer:tc(doko, run_query, [DomId, CatId, Query]),
    io:format("query took ~p ms~n", [Time/1000]).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

read_line(Io, DomId, Lang, CatId) ->
    case file:read_line(Io) of
        eof ->
            ok;
        {error, Reason} ->
            exit(Reason);
        {ok, Data} ->
            [DocId, Body] = re:split(Data ," ",[{return,list},{parts,2}]),
            doko:add_doc(DomId, CatId, DocId, list_to_binary(Body)),
            read_line(Io, DomId, Lang, CatId)
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
