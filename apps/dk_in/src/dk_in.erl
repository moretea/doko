-module(dk_in).

%% API
-export([add_doc/4]).

%% For internal use only
-export(['_add_doc'/4]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Adds a document.
add_doc(DomId, CatId, DocId, Doc) ->
    spawn(?MODULE, '_add_doc', [DomId, CatId, DocId, Doc]),
    ok.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

%% @private
'_add_doc'(DomId, CatId, DocId, Doc) ->
    {ok, Lang} = dk_meta:dom_lang(DomId),
    lists:foreach(
      fun (Term) -> add_post(DomId, CatId, DocId, Term) end,
      lists:usort(dk_pp:terms(Doc, Lang))),
    add_doc_id(DomId, CatId, DocId),
    ok.

add_post(DomId, CatId, DocId, Term) ->
    Nodes = dk_ring:whereis({invix_data, {DomId, CatId, Term}}),
    %% TO-DO:
    %% - choose timeout value
    %% - handle "bad" nodes
    Timeout = infinity,
    {_, []} = rpc:multicall(Nodes,
                            dk_ii, add_post, [DomId, CatId, DocId, Term],
                            Timeout),
    ok.

add_doc_id(DomId, CatId, DocId) ->
    Nodes = dk_ring:whereis({cat_data, {DomId, CatId}}),
    %% TO-DO:
    %% - choose timeout value
    %% - handle "bad" nodes
    Timeout = infinity,
    {_, []} = rpc:multicall(Nodes,
                            dk_ii, add_doc_id, [DomId, CatId, DocId],
                            Timeout),
    ok.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
