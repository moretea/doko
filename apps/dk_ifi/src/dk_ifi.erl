-module(dk_ifi).

-export([add_dom/1, del_dom/1, add_cat/2, del_cat/2]).
-export([add_post/4, del_post/4, get_posts_list/3]).

%%% API

add_dom(DomId) ->
    dk_ifi_sup:add_dom(DomId).

del_dom(DomId) ->
    dk_ifi_sup:del_dom(DomId).

add_cat(DomId, CatId) ->
    dk_ifi_dom_sup:add_cat(DomId, CatId).

del_cat(DomId, CatId) ->
    dk_ifi_dom_sup:del_cat(DomId, CatId).

add_post(DomId, CatId, DocId, Term) ->
    dk_ifi_cat:add_post(DomId, CatId, DocId, Term).

del_post(DomId, CatId, DocId, Term) ->
    dk_ifi_cat:del_post(DomId, CatId, DocId, Term).

get_posts_list(DomId, CatId, Term) ->
    dk_ifi_cat:get_posts_list(DomId, CatId, Term).

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
