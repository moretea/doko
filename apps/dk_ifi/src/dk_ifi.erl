-module(dk_ifi).
-export([add_dom, del_dom, add_cat, del_cat]).
-export([add_pointer, del_pointer, get_inv_list]).

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
