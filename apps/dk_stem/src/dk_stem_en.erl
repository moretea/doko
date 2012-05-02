%%% @doc English stemming library.
%%% @reference See <a
%%% href="http://snowball.tartarus.org/algorithms/english/stemmer.html"> this
%%% page</a> for more information about the algorithm used.

-module(dk_stem_en).
-include("../../dk_utf8/include/dk_utf8.hrl").

%% API
-export([stem/1]).

%% Marco definitions
-define(VOWELS, <<"aeiouy">>).

%%% API functions

-spec stem(utf8_string()) -> utf8_string().
stem(<<"andes">>) -> <<"andes">>;
stem(<<"atlas">>) -> <<"atlas">>;
stem(<<"bias">>) -> <<"bias">>;
stem(<<"cosmos">>) -> <<"cosmos">>;
stem(<<"dying">>) -> <<"die">>;
stem(<<"early">>) -> <<"earli">>;
stem(<<"fluently">>) -> <<"fluentli">>;
stem(<<"gently">>) -> <<"gentl">>;
stem(<<"howe">>) -> <<"howe">>;
stem(<<"idly">>) -> <<"idl">>;
stem(<<"lying">>) -> <<"lie">>;
stem(<<"news">>) -> <<"news">>;
stem(<<"only">>) -> <<"onli">>;
stem(<<"singly">>) -> <<"singl">>;
stem(<<"skies">>) -> <<"sky">>;
stem(<<"sky">>) -> <<"sky">>;
stem(<<"tying">>) -> <<"tie">>;
stem(<<"ugly">>) -> <<"ugli">>;
stem(Word)  ->
    Length = dk_utf8:length(Word),
    if Length =< 2 -> Word;
       true ->
            NewWord = step1a(step0(preprocess(Word))),
            case NewWord of
                <<"canning">> -> NewWord;
                <<"earring">> -> NewWord;
                <<"exceed">> -> NewWord;
                <<"herring">> -> NewWord;
                <<"inning">> -> NewWord;
                <<"outing">> -> NewWord;
                <<"proceed">> -> NewWord;
                <<"succeed">> -> NewWord;
                _ ->
                    postprocess(
                      step5(step4(step3(step2(step1c(step1b(NewWord)))))))
            end
    end.

%%% Internal functions

step0(Word) ->
    Apo_s_apo = dk_utf8:suffix(<<"'s'">>, Word),
    Apo_s = dk_utf8:suffix(<<"'s">>, Word),
    Apo = dk_utf8:suffix(<<"'">>, Word),
    if
        Apo_s_apo -> dk_utf8:substr(Word, 1, -3);
        Apo_s -> dk_utf8:substr(Word, 1, -2);
        Apo -> dk_utf8:substr(Word, 1, -1);
        true -> Word
    end.

step1a(Word) ->
    Sses = dk_utf8:suffix(<<"sses">>, Word),
    Ied = dk_utf8:suffix(<<"ied">>, Word),
    Ies = dk_utf8:suffix(<<"ies">>, Word),
    Us = dk_utf8:suffix(<<"us">>, Word),
    Ss = dk_utf8:suffix(<<"ss">>, Word),
    S = dk_utf8:suffix(<<"s">>, Word),
    if
        Sses ->
            dk_utf8:substr(Word, 1, -2);
        Ied or Ies ->
            case dk_utf8:length(Word) of
                3 -> dk_utf8:substr(Word, 1, 2);
                4 -> dk_utf8:substr(Word, 1, 3);
                _ -> dk_utf8:substr(Word, 1, -2)
            end;
        Us or Ss ->
            Word;
        S ->
            case vowel(dk_utf8:substr(Word, 1, -2)) of
                true -> dk_utf8:substr(Word, 1, -1);
                false -> Word
            end;
        true ->
            Word
    end.

step1b(Word) ->
    {R1, _} = r1_r2(Word),
    Eed = dk_utf8:suffix(<<"eed">>, Word),
    Eedly = dk_utf8:suffix(<<"eedly">>, Word),
    Ed = dk_utf8:suffix(<<"ed">>, Word),
    Edly = dk_utf8:suffix(<<"edly">>, Word),
    Ing = dk_utf8:suffix(<<"ing">>, Word),
    Ingly = dk_utf8:suffix(<<"ingly">>, Word),
    if
        Eed -> 
            case dk_utf8:suffix(<<"eed">>, R1) of
                true -> dk_utf8:substr(Word, 1, -1);
                false -> Word
            end;
        Eedly ->
            case dk_utf8:suffix(<<"eedly">>, R1) of
                true -> dk_utf8:substr(Word, 1, -3);
                false -> Word
            end;
        Ed -> del_1b(Word, -2);
        Edly -> del_1b(Word, -4);
        Ing -> del_1b(Word, -3);
        Ingly -> del_1b(Word, -5);
        true -> Word
    end.

del_1b(Word, N) ->
    NewWord = dk_utf8:substr(Word, 1, N),
    case vowel(NewWord) of
        false -> Word;
        true ->
            At = dk_utf8:suffix(<<"at">>, NewWord),
            Bl = dk_utf8:suffix(<<"bl">>, NewWord),
            Iz = dk_utf8:suffix(<<"iz">>, NewWord),
            case At or Bl or Iz of
                true -> <<NewWord/bytes, "e">>;
                false ->
                    case double_end(NewWord) of
                        true -> dk_utf8:substr(NewWord, 1, -1);
                        false ->
                            case short(NewWord) of
                                true -> <<NewWord/bytes, "e">>;
                                false -> NewWord
                            end
                    end
            end
    end.

step1c(Word) ->
    case dk_utf8:length(Word) of
        2 -> Word;
        _ ->
            Regex = [<<"[^">>, ?VOWELS, <<"][yY]$">>],
            Options = [unicode, {capture, none}],
            case re:run(Word, Regex, Options) of
                match -> <<(dk_utf8:substr(Word, 1, -1))/bytes, "i">>;
                nomatch -> Word
            end
    end.

step2(Word) ->
    List = [{<<"ational">>, <<"ate">>},
            {<<"fulness">>, <<"ful">>},
            {<<"iveness">>, <<"ive">>},
            {<<"ization">>, <<"ize">>},
            {<<"ousness">>, <<"ous">>},
            {<<"biliti">>, <<"ble">>},
            {<<"lessli">>, <<"less">>},
            {<<"tional">>, <<"tion">>},
            {<<"alism">>, <<"al">>},
            {<<"aliti">>, <<"al">>},
            {<<"ation">>, <<"ate">>},
            {<<"entli">>, <<"ent">>},
            {<<"fulli">>, <<"ful">>},
            {<<"iviti">>, <<"ive">>},
            {<<"ousli">>, <<"ous">>},
            {<<"abli">>, <<"able">>},
            {<<"alli">>,  <<"al">>},
            {<<"anci">>, <<"ance">>},
            {<<"ator">>, <<"ate">>},
            {<<"enci">>, <<"ence">>},
            {<<"izer">>, <<"ize">>},
            {<<"bli">>, <<"ble">>}],
    {R1, _} = r1_r2(Word),
    Pred = fun({Suffix, _}) -> not dk_utf8:suffix(Suffix, R1) end,
    case lists:dropwhile(Pred, List) of
        [{Suffix, NewSuffix} | _] ->
            Prefix = dk_utf8:substr(Word, 1, -(dk_utf8:length(Suffix))),
            <<Prefix/bytes, NewSuffix/bytes>>;
        [] ->
            Ogi = dk_utf8:suffix(<<"ogi">>, R1),
            Logi = dk_utf8:suffix(<<"logi">>, Word),
            if
                Ogi and Logi ->
                    dk_utf8:substr(Word, 1, -1);
                true ->
                    Suffix = dk_utf8:suffix(<<"li">>, R1),
                    NewWord = dk_utf8:substr(Word, 1, -2),
                    Ending = valid_li_ending(NewWord),
                    case {Suffix, Ending} of
                        {true, true} -> NewWord;
                        _ -> Word
                    end
            end
    end.

step3(Word) ->
    List = [{<<"ational">>, <<"ate">>},
            {<<"tional">>, <<"tion">>},
            {<<"alize">>, <<"al">>},
            {<<"icate">>, <<"ic">>},
            {<<"iciti">>, <<"ic">>},
            {<<"ical">>, <<"ic">>},
            {<<"ness">>, <<>>},
            {<<"ful">>, <<>>}],
    {R1, R2} = r1_r2(Word),
    Pred = fun({Suffix, _}) -> not dk_utf8:suffix(Suffix, R1) end,
    case lists:dropwhile(Pred, List) of
        [{Suffix, NewSuffix} | _] ->
            Prefix = dk_utf8:substr(Word, 1, -(dk_utf8:length(Suffix))),
            <<Prefix/bytes, NewSuffix/bytes>>;
        [] ->
            case dk_utf8:suffix(<<"ative">>, R2) of
                true -> dk_utf8:substr(Word, 1, -5);
                _ -> Word
            end
    end.

step4(Word) ->
    List = [<<"ement">>, <<"able">>, <<"ance">>, <<"ence">>, <<"ible">>,
            <<"ment">>, <<"ant">>, <<"ate">>, <<"ent">>, <<"ism">>, <<"iti">>,
            <<"ive">>, <<"ize">>, <<"ous">>, <<"al">>, <<"er">>, <<"ic">>],
    {_, R2} = r1_r2(Word),
    Pred = fun(Suffix) -> not dk_utf8:suffix(Suffix, Word) end,
    case lists:dropwhile(Pred, List) of
        [Suffix | _] ->
            case dk_utf8:suffix(Suffix, R2) of
                true ->
                    dk_utf8:substr(Word, 1, -(dk_utf8:length(Suffix)));
                false ->
                    Word
            end;
        [] ->
            Ion = dk_utf8:suffix(<<"ion">>, R2),
            Tion = dk_utf8:suffix(<<"tion">>, Word),
            Sion = dk_utf8:suffix(<<"sion">>, Word),
            if
                Ion and (Tion or Sion) -> dk_utf8:substr(Word, 1, -3);
                true -> Word
            end
    end.

step5(Word) ->
    {R1, R2} = r1_r2(Word),
    R1E = dk_utf8:suffix(<<"e">>, R1),
    R2E = dk_utf8:suffix(<<"e">>, R2),
    SuffixL = dk_utf8:suffix(<<"l">>, R2),
    DoubleL = dk_utf8:suffix(<<"ll">>, Word),
    if
        SuffixL and DoubleL ->
            dk_utf8:substr(Word, 1, -1);
        R2E ->
            dk_utf8:substr(Word, 1, -1);
        R1E ->
            NewWord = dk_utf8:substr(Word, 1, -1),
            case short_syllable(dk_utf8:substr(NewWord, -3)) of 
                false -> dk_utf8:substr(Word, 1, -1);
                true -> Word
            end;
        true ->
            Word
    end.

valid_li_ending(Word) ->
    lists:member(dk_utf8:substr(Word, -1),
                 [<<"c">>, <<"d">>, <<"e">>, <<"g">>, <<"h">>, <<"k">>,
                  <<"m">>, <<"n">>, <<"r">>, <<"t">>]).

short(Word) ->
    {R1, _} = r1_r2(Word),
    Regex1 = [<<"[^">>, ?VOWELS, <<"][">>, ?VOWELS, <<"][^">>, ?VOWELS,
              <<"wxY]$">>],
    Regex2 = [<<"^[">>, ?VOWELS, <<"][^">>, ?VOWELS, <<"]$">>],
    Options = [unicode, {capture, none}],
    case {R1, re:run(Word, Regex1, Options), re:run(Word, Regex2, Options)} of
        {<<>>, match, _} -> true;
        {<<>>, _, match} -> true;
        _ -> false
    end.

short_syllable(<<C1/utf8, C2/utf8, C3/utf8>>) ->
    (not vowel(C1)) and vowel(C2) and (not vowel(C3)) and (C3 /= $w) and
        (C3 /= $x) and (C3 /= $Y);
short_syllable(<<C1/utf8, C2/utf8>>) ->
    vowel(C1) and not vowel(C2).

double_end(Word) ->
    lists:member(dk_utf8:substr(Word, -2),
                 [<<"bb">>, <<"dd">>, <<"ff">>, <<"gg">>, <<"mm">>, <<"nn">>,
                  <<"pp">>, <<"rr">>, <<"tt">>]).

vowel(<<>>) ->
    false;
vowel(<<Char/utf8, Rest/bytes>>) ->
    vowel(Char) orelse vowel(Rest);
vowel(Char) ->
   case Char of
       $a -> true;
       $e -> true;
       $i -> true;
       $o -> true;
       $u -> true;
       $y -> true;
       _ -> false
    end.

preprocess(<<"'", Rest/bytes>>) ->
    uppercase_y_chars(<<Rest/bytes>>);
preprocess(Word) ->
    uppercase_y_chars(Word).

uppercase_y_chars(Word) ->
    Word2 = case Word of
                <<"y" , Rest/bytes>> -> <<"Y", Rest/bytes>>;
                _ -> Word
            end,
    Regex = [<<"([">>, ?VOWELS, <<"])y">>],
    Options = [unicode, global, {return, binary}],
    re:replace(Word2, Regex, "\\1Y", Options).

postprocess(Word) ->
    Regex = [<<"Y">>],
    Options = [unicode, global, {return, binary}],
    re:replace(Word, Regex, "y", Options).

-spec r1_r2(utf8_string()) -> {utf8_string(), utf8_string()}.
r1_r2(Word) ->
    Regex = [<<".*?[">>, ?VOWELS, <<"][^">>, ?VOWELS, <<"](.*)$">>],
    Options = [unicode, global, {capture, all_but_first, binary}],
    R1 = case Word of
             <<"arsen", Rest/bytes>> ->
                 Rest;
             <<"commun", Rest/bytes>> ->
                 Rest;
             <<"gener", Rest/bytes>> ->
                 Rest;
             _ ->
                 case re:run(Word, Regex, Options) of
                     {match, [[Match1]]} -> Match1;
                     _ -> <<>>
                 end
         end,
    R2 = case re:run(R1, Regex, Options) of
             {match, [[Match2]]} -> Match2;
             _ -> <<>>
         end,
    {R1, R2}.

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
