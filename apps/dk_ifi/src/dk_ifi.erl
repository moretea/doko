-module(dk_ifi).
-export([add_dom/1, del_dom/1, add_cat/2, del_cat/2]).
-export([add_pointer/4, del_pointer/4, get_inv_list/3]).

add_dom(_DomId) ->
    ok.

del_dom(_DomId) ->
    ok.

add_cat(_DomId, _CatId) ->
    ok.

del_cat(_DomId, _CatId) ->
    ok.

add_pointer(_DomId, _CatId, _DocId, _Term) ->
    ok.

del_pointer(_DomId, _CatId, _DocId, _Term) ->
    ok.

get_inv_list(_DomId, _CatId, _Term) ->
    [].

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
