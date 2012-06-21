-module(doko_node).

%% API
-export([add_doc_id/2,del_doc_id/2,doc_ids/1]).
-export([start/0,stop/0]).
%% -export([load/1]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

add_doc_id(Term, DocId) ->
    doko_index:add_doc_id(Term, DocId).

del_doc_id(Term, DocId) ->
    doko_index:del_doc_id(Term, DocId).

doc_ids(Term) ->
    doko_index:doc_ids(Term).

%% @doc Starts a node.
start() ->
    application:start(doko_node).

%% @doc Stops the node.
stop() ->
    application:stop(doko_node).

%% load(File) ->
%%     {ok,Binary} = file:read_file(File),
%%     add_docs(Binary),
%%     ok.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

%% add_docs(<<>>) ->
%%     ok;
%% add_docs(Binary) ->
%%     [Line,Rest] = binary:split(Binary, <<"\n">>), % does this work correctly
%%                                                   % with UTF-8?
%%     [Id,Text] = binary:split(Line, <<" ">>),
%%     IntId = list_to_integer(binary_to_list(Id)),
%%     plists:foreach(fun (T) -> doko_index:add_doc_id(T, IntId) end,
%%                    doko_preprocessing:terms(Text, "en")),
%%     add_docs(Rest).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
