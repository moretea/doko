-module(dk_stem_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(value(Key,Config), proplists:get_value(Key,Config)).

all() ->
    [{group, integration_tests}].

groups() ->
    [{integration_tests, [test_en, test_nl]}].

test_en(Config) ->
    vocab_stemmed("en", Config),
    ok.

test_nl(Config) ->
    vocab_stemmed("nl", Config),
    ok.

vocab_stemmed(Lang, Config) ->
    DataDir = ?value(data_dir, Config),
    {ok, Binary} =
        file:read_file(DataDir ++ "vocabulary_stemmed_" ++ Lang ++ ".txt"),
    Lines = re:split(Binary, <<"\n">>, [trim]),
    {ok, Re} = re:compile(<<"^([^ ]*) *([^ ]*)$">>, [unicode]),
    Options = [{capture, all_but_first, binary}],
    Mod = list_to_atom("dk_stem_" ++ Lang),
    lists:foreach(
      fun(Line) ->
              {match, [Word, Stem]} = re:run(Line, Re, Options),
              Stem = Mod:stem(Word)
      end,
      Lines).

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
