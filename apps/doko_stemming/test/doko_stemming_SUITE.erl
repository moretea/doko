-module(doko_stemming_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Tests
-export([test_en/1, test_nl/1]).

%% CT functions
-export([all/0, groups/0]).

%%----------------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------------

test_en(Config) ->
    vocab_stemmed("en", Config),
    ok.

test_nl(Config) ->
    vocab_stemmed("nl", Config),
    ok.

%%----------------------------------------------------------------------------
%% CT functions
%%----------------------------------------------------------------------------

all() ->
    [{group,default}].

groups() ->
    [{default,[parallel],[test_en, test_nl]}].

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

vocab_stemmed(Lang, Config) ->
    DataDir = ?config(data_dir, Config),
    {ok,Binary} =
        file:read_file(DataDir ++ "vocabulary_stemmed_" ++ Lang ++ ".txt"),
    Lines = re:split(Binary, <<"\n">>, [trim]),
    {ok,Re} = re:compile(<<"^([^ ]*) *([^ ]*)$">>, [unicode]),
    Options = [{capture,all_but_first,binary}],
    Mod = list_to_atom("doko_stemming_" ++ Lang),
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
