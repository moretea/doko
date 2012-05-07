-module(dk_utf8_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [{group, unit_tests}].

groups() ->
    [{unit_tests, [parallel],
      [test_length,
       test_has_mb_char, test_suffix,
       test_reverse,
       test_substr]}].

test_length(_Config) ->
    Word = unicode:characters_to_binary("à", latin1),
    1 = dk_utf8:length(Word),
    ok.

test_has_mb_char(_Config) ->
    true = dk_utf8:has_mb_char(
             unicode:characters_to_binary("prêt-à-porter", latin1)),
    false = dk_utf8:has_mb_char(<<"au bain marie">>),
    ok.

test_suffix(_Config) ->
    Phrase = unicode:characters_to_binary("à la carte", latin1),
    true = dk_utf8:suffix(<<"carte">>, Phrase),
    false = dk_utf8:suffix(<<"card">>, Phrase),
    ok.

test_reverse(_Config) ->
    Word = unicode:characters_to_binary("prêt-à-porter", latin1),
    Drow = unicode:characters_to_binary("retrop-à-têrp", latin1),
    Drow = dk_utf8:reverse(Word),
    ok.

test_substr(_Config) ->
    Phrase = unicode:characters_to_binary("à la carte", latin1),
    %%                                     123456789
    %%                                      987654321
    <<"la carte">> = dk_utf8:substr(Phrase, 3),
    <<"la">> = dk_utf8:substr(Phrase, 3, 2),
    <<"carte">> = dk_utf8:substr(Phrase, -5),
    <<" la ">> = dk_utf8:substr(Phrase, 2, -5),
    <<"r">> = dk_utf8:substr(Phrase, -3, -2),
    <<>> = dk_utf8:substr(Phrase, -2, -3),
    ok.

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
