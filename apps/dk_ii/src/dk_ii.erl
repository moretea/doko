-module(dk_ii).

%% API
-export([add_dom/1, del_dom/1, add_cat/2, del_cat/2]).
-export([add_post/4, del_post/4, get_posts/3]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Adds a domain.
add_dom(DomId) ->
    dk_ii_sup:add_dom(DomId).

%% @doc Deletes a domain.
del_dom(DomId) ->
    dk_ii_sup:del_dom(DomId).

%% @doc Adds a category.
add_cat(DomId, CatId) ->
    dk_ii_dom_sup:add_cat(DomId, CatId).

%% @doc Deletes a category.
del_cat(DomId, CatId) ->
    dk_ii_dom_sup:del_cat(DomId, CatId).

%% @doc Adds a posting.
add_post(DomId, CatId, DocId, Term) ->
    dk_ii_cat:add_post(DomId, CatId, DocId, Term).

%% @doc Deletes a posting.
del_post(DomId, CatId, DocId, Term) ->
    dk_ii_cat:del_post(DomId, CatId, DocId, Term).

%% @doc Returns the inversed list for a term.
get_posts(DomId, CatId, Term) ->
    dk_ii_cat:get_posts(DomId, CatId, Term).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
