-module(dk_ifi).
-export([add_dom/1, del_dom/1, add_cat/2, del_cat/2]).
-export([add_pointer/4, del_pointer/4, get_inv_list/3]).

add_dom(DomId) ->
    ok.

del_dom(DomId) ->
    ok.

add_cat(DomId, CatId) ->
    ok.

del_cat(DomId, CatId)
    ok.

add_pointer(DomId, CatId, DocId, Term) ->
    ok.

del_pointer(DomId, CatId, DocId, Term) ->
    ok.

get_inv_list(DomId, CatId, Term) ->
    [].

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
