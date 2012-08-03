-module(doko_utf8_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------------

length_test() ->
    Word = unicode:characters_to_binary("à", latin1),
    ?assertEqual(1, doko_utf8:length(Word)).

has_mb_char_test_() ->
    Phrase = unicode:characters_to_binary("prêt-à-porter", latin1),
    [?_assert(doko_utf8:has_mb_char(Phrase)),
     ?_assert(not doko_utf8:has_mb_char(<<"au bain marie">>))].

suffix_test_() ->
    Phrase = unicode:characters_to_binary("à la carte", latin1),
    [?_assert(doko_utf8:suffix(<<"carte">>, Phrase)),
     ?_assert(not doko_utf8:suffix(<<"card">>, Phrase))].

reverse_test() ->
    Word = unicode:characters_to_binary("prêt-à-porter", latin1),
    Drow = unicode:characters_to_binary("retrop-à-têrp", latin1),
    ?assertEqual(Drow, doko_utf8:reverse(Word)).

substr_test_() ->
    Phrase = unicode:characters_to_binary("à la carte", latin1),
    %%                                     0123456789
    %%                                      987654321
    [?_assertEqual(<<"la carte">>, doko_utf8:substr(Phrase, 2)),
     ?_assertEqual(<<"la">>, doko_utf8:substr(Phrase, 2, 2)),
     ?_assertEqual(<<"carte">>, doko_utf8:substr(Phrase, -5)),
     ?_assertEqual(<<" la ">>, doko_utf8:substr(Phrase, 1, -5)),
     ?_assertEqual(<<"r">>, doko_utf8:substr(Phrase, -3, -2)),
     ?_assertEqual(<<>>, doko_utf8:substr(Phrase, -2, -3))].

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
