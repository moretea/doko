%%----------------------------------------------------------------------------
%% @doc English stemming library.
%% @reference See <a
%% href="http://snowball.tartarus.org/algorithms/english/stemmer.html"> this
%% page</a> for more information about the algorithm used.
%% @end
%%----------------------------------------------------------------------------

-module(doko_stemming_en).
-include("../../doko_utf8/include/doko_utf8.hrl").

%% API
-export([stem/1]).

%% Marco definitions
-define(VOWELS, <<"aeiouy">>).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

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
    Length = doko_utf8:length(Word),
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

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

step0(Word) ->
    Apo_s_apo = doko_utf8:suffix(<<"'s'">>, Word),
    Apo_s = doko_utf8:suffix(<<"'s">>, Word),
    Apo = doko_utf8:suffix(<<"'">>, Word),
    if
        Apo_s_apo -> doko_utf8:substr(Word, 0, -3);
        Apo_s -> doko_utf8:substr(Word, 0, -2);
        Apo -> doko_utf8:substr(Word, 0, -1);
        true -> Word
    end.

step1a(Word) ->
    Sses = doko_utf8:suffix(<<"sses">>, Word),
    Ied = doko_utf8:suffix(<<"ied">>, Word),
    Ies = doko_utf8:suffix(<<"ies">>, Word),
    Us = doko_utf8:suffix(<<"us">>, Word),
    Ss = doko_utf8:suffix(<<"ss">>, Word),
    S = doko_utf8:suffix(<<"s">>, Word),
    if
        Sses ->
            doko_utf8:substr(Word, 0, -2);
        Ied or Ies ->
            case doko_utf8:length(Word) of
                3 -> doko_utf8:substr(Word, 0, 2);
                4 -> doko_utf8:substr(Word, 0, 3);
                _ -> doko_utf8:substr(Word, 0, -2)
            end;
        Us or Ss ->
            Word;
        S ->
            case vowel(doko_utf8:substr(Word, 0, -2)) of
                true -> doko_utf8:substr(Word, 0, -1);
                false -> Word
            end;
        true ->
            Word
    end.

step1b(Word) ->
    {R1, _} = r1_r2(Word),
    Eed = doko_utf8:suffix(<<"eed">>, Word),
    Eedly = doko_utf8:suffix(<<"eedly">>, Word),
    Ed = doko_utf8:suffix(<<"ed">>, Word),
    Edly = doko_utf8:suffix(<<"edly">>, Word),
    Ing = doko_utf8:suffix(<<"ing">>, Word),
    Ingly = doko_utf8:suffix(<<"ingly">>, Word),
    if
        Eed -> 
            case doko_utf8:suffix(<<"eed">>, R1) of
                true -> doko_utf8:substr(Word, 0, -1);
                false -> Word
            end;
        Eedly ->
            case doko_utf8:suffix(<<"eedly">>, R1) of
                true -> doko_utf8:substr(Word, 0, -3);
                false -> Word
            end;
        Ed -> del_1b(Word, -2);
        Edly -> del_1b(Word, -4);
        Ing -> del_1b(Word, -3);
        Ingly -> del_1b(Word, -5);
        true -> Word
    end.

del_1b(Word, N) ->
    NewWord = doko_utf8:substr(Word, 0, N),
    case vowel(NewWord) of
        false -> Word;
        true ->
            At = doko_utf8:suffix(<<"at">>, NewWord),
            Bl = doko_utf8:suffix(<<"bl">>, NewWord),
            Iz = doko_utf8:suffix(<<"iz">>, NewWord),
            case At or Bl or Iz of
                true -> <<NewWord/bytes, "e">>;
                false ->
                    case double_end(NewWord) of
                        true -> doko_utf8:substr(NewWord, 0, -1);
                        false ->
                            case short(NewWord) of
                                true -> <<NewWord/bytes, "e">>;
                                false -> NewWord
                            end
                    end
            end
    end.

step1c(Word) ->
    case doko_utf8:length(Word) of
        2 -> Word;
        _ ->
            Regex = [<<"[^">>, ?VOWELS, <<"][yY]$">>],
            Options = [unicode, {capture, none}],
            case re:run(Word, Regex, Options) of
                match -> <<(doko_utf8:substr(Word, 0, -1))/bytes, "i">>;
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
    Pred = fun({Suffix, _}) -> not doko_utf8:suffix(Suffix, R1) end,
    case lists:dropwhile(Pred, List) of
        [{Suffix, NewSuffix} | _] ->
            Prefix = doko_utf8:substr(Word, 0, -(doko_utf8:length(Suffix))),
            <<Prefix/bytes, NewSuffix/bytes>>;
        [] ->
            Ogi = doko_utf8:suffix(<<"ogi">>, R1),
            Logi = doko_utf8:suffix(<<"logi">>, Word),
            if
                Ogi and Logi ->
                    doko_utf8:substr(Word, 0, -1);
                true ->
                    Suffix = doko_utf8:suffix(<<"li">>, R1),
                    NewWord = doko_utf8:substr(Word, 0, -2),
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
    Pred = fun({Suffix, _}) -> not doko_utf8:suffix(Suffix, R1) end,
    case lists:dropwhile(Pred, List) of
        [{Suffix, NewSuffix} | _] ->
            Prefix = doko_utf8:substr(Word, 0, -(doko_utf8:length(Suffix))),
            <<Prefix/bytes, NewSuffix/bytes>>;
        [] ->
            case doko_utf8:suffix(<<"ative">>, R2) of
                true -> doko_utf8:substr(Word, 0, -5);
                _ -> Word
            end
    end.

step4(Word) ->
    List = [<<"ement">>, <<"able">>, <<"ance">>, <<"ence">>, <<"ible">>,
            <<"ment">>, <<"ant">>, <<"ate">>, <<"ent">>, <<"ism">>, <<"iti">>,
            <<"ive">>, <<"ize">>, <<"ous">>, <<"al">>, <<"er">>, <<"ic">>],
    {_, R2} = r1_r2(Word),
    Pred = fun(Suffix) -> not doko_utf8:suffix(Suffix, Word) end,
    case lists:dropwhile(Pred, List) of
        [Suffix | _] ->
            case doko_utf8:suffix(Suffix, R2) of
                true ->
                    doko_utf8:substr(Word, 0, -(doko_utf8:length(Suffix)));
                false ->
                    Word
            end;
        [] ->
            Ion = doko_utf8:suffix(<<"ion">>, R2),
            Tion = doko_utf8:suffix(<<"tion">>, Word),
            Sion = doko_utf8:suffix(<<"sion">>, Word),
            if
                Ion and (Tion or Sion) -> doko_utf8:substr(Word, 0, -3);
                true -> Word
            end
    end.

step5(Word) ->
    {R1, R2} = r1_r2(Word),
    R1E = doko_utf8:suffix(<<"e">>, R1),
    R2E = doko_utf8:suffix(<<"e">>, R2),
    SuffixL = doko_utf8:suffix(<<"l">>, R2),
    DoubleL = doko_utf8:suffix(<<"ll">>, Word),
    if
        SuffixL and DoubleL ->
            doko_utf8:substr(Word, 0, -1);
        R2E ->
            doko_utf8:substr(Word, 0, -1);
        R1E ->
            NewWord = doko_utf8:substr(Word, 0, -1),
            case short_syllable(doko_utf8:substr(NewWord, -3)) of 
                false -> doko_utf8:substr(Word, 0, -1);
                true -> Word
            end;
        true ->
            Word
    end.

valid_li_ending(Word) ->
    lists:member(doko_utf8:substr(Word, -1),
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
    vowel(C1) and not vowel(C2);
short_syllable(_) ->
    true.

double_end(Word) ->
    lists:member(doko_utf8:substr(Word, -2),
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

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
