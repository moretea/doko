%%% @doc Dutch stemming library.
%%% @reference See <a
%%% href="http://snowball.tartarus.org/algorithms/dutch/stemmer.html"> this
%%% page</a> for more information about the algorithm used.

-module(dk_stem_nl).
-include("../../dk_utf8/include/dk_utf8.hrl").

%% API
-export([stem/1]).

%% Marco definitions
-define(VOWELS, <<"aeiouy", 16#C3A8:16>>).

%%% API functions

-spec stem(utf8_string()) -> utf8_string().
stem(Word)  ->
    Length = dk_utf8:length(Word),
    if
        Length =< 3 ->
            remove_umlauts(remove_accents(Word));
        true ->
            lowercase_i_and_y_chars(
              step4(step3b(step3a(step2(step1(preprocess(Word)))))))
    end.

%%% Internal functions

step1(Word) ->
    {StdR1, _R2} = standard_r1_r2(Word),
    R1 = adjust_r1(Word, Word, StdR1),
    Heden = dk_utf8:suffix(<<"heden">>, R1),
    Ene = dk_utf8:suffix(<<"ene">>, R1),
    En = dk_utf8:suffix(<<"en">>, R1),
    Se = dk_utf8:suffix(<<"se">>, R1),
    S = dk_utf8:suffix(<<"s">>, R1),
    if
        Heden ->
            <<(dk_utf8:substr(Word, 1, -5))/bytes, "heid">>;
        Ene or En ->
            WordHeden = dk_utf8:suffix(<<"heden">>, Word),
            NewWord = if
                          En -> dk_utf8:substr(Word, 1, -2);
                          Ene -> dk_utf8:substr(Word, 1, -3)
                      end,
            ValidEnding = valid_en_ending(NewWord),
            if
                WordHeden or not ValidEnding -> Word;
                true -> undouble_end(NewWord)
            end;
        Se or S ->
            NewWord = if
                          S -> dk_utf8:substr( Word, 1, -1);
                          Se -> dk_utf8:substr( Word, 1, -2)
                      end,
            ValidEnding = valid_s_ending(NewWord),
            if
                ValidEnding -> NewWord;
                true -> Word
            end;
        true ->
            Word
    end.

step2(Word) ->
    {StdR1, _R2} = standard_r1_r2(Word),
    R1 = adjust_r1(Word, Word, StdR1),
    SuffixE = dk_utf8:suffix(<<"e">>, R1),
    if
        not SuffixE ->
            {Word, false};
        true ->
            Regex = [<<"[^">>, ?VOWELS, <<"]e$">>],
            Options = [unicode],
            case re:run(Word, Regex, Options) of
                nomatch ->
                    {Word, false};
                _ ->
                    NewWord = undouble_end(dk_utf8:substr(Word, 1, -1)),
                    {NewWord, true}
            end
    end.

step3a({Word, Step2Success}) ->
    {StdR1, R2} = standard_r1_r2(Word),
    R1 = adjust_r1(Word, Word, StdR1),
    Heid = dk_utf8:suffix(<<"heid">>, R2),
    Cheid = dk_utf8:suffix(<<"cheid">>, Word),
    if
        (not Heid) or Cheid ->
            {Word, Step2Success};
        true ->
            NewWord = dk_utf8:substr(Word, 1, -4),
            NewR1 = dk_utf8:substr(R1, 1, -4),
            En = dk_utf8:suffix(<<"en">>, NewR1),
            if
                not En ->
                    {NewWord, Step2Success};
                true ->
                     ValidEnding =
                        valid_en_ending(dk_utf8:substr(NewR1, 1, -2)),
                    if
                        not ValidEnding ->
                            {NewWord, Step2Success};
                        true ->
                            NewNewWord = undouble_end(
                                           dk_utf8:substr(NewWord, 1, -2)),
                            {NewNewWord, Step2Success}
                    end
            end
    end.

step3b({Word, Step2Success}) ->
    {_StdR1, R2} = standard_r1_r2(Word),
    End = dk_utf8:suffix(<<"end">>, R2),
    Ing = dk_utf8:suffix(<<"ing">>, R2),
    Ig = dk_utf8:suffix(<<"ig">>, R2),
    Eig = dk_utf8:suffix(<<"eig">>, Word),
    Lijk = dk_utf8:suffix(<<"lijk">>, R2),
    Baar = dk_utf8:suffix(<<"baar">>, R2),
    Bar = dk_utf8:suffix(<<"bar">>, R2),
    if
        End or Ing ->
            NewWord = dk_utf8:substr(Word, 1, -3),
            NewR2 = dk_utf8:substr(R2, 1, -3),
            Ig2 = dk_utf8:suffix(<<"ig">>, NewR2),
            Eig2 = dk_utf8:suffix(<<"eig">>, NewWord),
            if
                Ig2 and not Eig2 ->
                    dk_utf8:substr(NewWord, 1, -2);
                true ->
                    undouble_end(NewWord)
            end;
        Ig and not Eig ->
            dk_utf8:substr(Word, 1, -2);
        Lijk ->
            {NewWord, _} = step2(dk_utf8:substr(Word, 1, -4)),
            NewWord;
        Baar ->
            dk_utf8:substr(Word, 1, -4);
        Bar and Step2Success ->
            dk_utf8:substr(Word, 1, -3);
        true ->
            Word
    end.

step4(Word) ->
    Regex = [<<"^(.*)([^">>, ?VOWELS, <<"])(aa|ee|oo|uu)">>,
             <<"([^I">>, ?VOWELS, <<"])$">>],
    Options = [unicode, global, {capture, all_but_first, binary}],
    case re:run(Word, Regex, Options) of
        {match, [[Begin, C, <<V/utf8, _>>, D]]} ->
            <<Begin/bytes, C/bytes, V, D/bytes>>;
        _ ->
            Word
    end.

standard_r1_r2(Word) ->
    Regex = [<<".*?[">>, ?VOWELS, <<"][^">>, ?VOWELS, <<"](.*)$">>],
    Options = [unicode, global, {capture, all_but_first, binary}],
    R1 = case re:run(Word, Regex, Options) of
             {match, [[Match1]]} -> Match1;
             _ -> <<>>
         end,
    R2 = case re:run(R1, Regex, Options) of
             {match, [[Match2]]} -> Match2;
             _ -> <<>>
         end,
    {R1, R2}.

adjust_r1(Word, <<V/utf8, C/utf8, L1/utf8, L2/utf8>>, R1) ->
    Test = vowel(V) and not vowel(C),
    case Test of
        true -> dk_utf8:substr(Word, 4);
        false -> adjust_r1(Word, <<C/utf8, L1/utf8, L2/utf8>>, R1)
    end;
adjust_r1(Word, <<V/utf8, C/utf8, _/utf8>>, R1) ->
    Test = vowel(V) and not vowel(C),
    case Test of
        true -> dk_utf8:substr(Word, 4);
        false -> R1
    end;
adjust_r1(Word, <<_/utf8, Rest/bytes>>, R1) ->
    adjust_r1(Word, <<Rest/bytes>>, R1);
adjust_r1(_, <<>>, R1) ->
    R1.

vowel(C) ->
   case C of
       $a -> true;
       $e -> true;
       $i -> true;
       $o -> true;
       $u -> true;
       $y -> true;
       $è -> true;
       _ -> false
    end.

valid_s_ending(Word) ->
    Drow = dk_utf8:reverse(Word),
    case Drow of
        <<"j"/utf8, _/bytes>> -> false;
        <<C/utf8, _/bytes>> -> not vowel(C)
    end.

valid_en_ending(<<>>) ->
    false;
valid_en_ending(Word) ->
    Drow = dk_utf8:reverse(Word),
    case Drow of
        <<"meg", _/bytes>> -> false;
        <<C/utf8, _/bytes>> -> not vowel(C)
    end.

undouble_end(Word) ->
    DoubleEnd = dk_utf8:suffix(<<"kk">>, Word) or
                dk_utf8:suffix(<<"dd">>, Word) or
                dk_utf8:suffix(<<"tt">>, Word),
    if
        DoubleEnd -> dk_utf8:substr(Word, 1, -1);
        true -> Word
    end.

preprocess(Word) ->
    uppercase_y_chars(
      uppercase_i_chars(remove_accents(remove_umlauts(Word)))).

remove_umlauts(Word) ->
    << <<(remove_umlaut(C))/utf8>> || <<C/utf8>> <= Word >>.

remove_umlaut(C) ->
    case C of
        16#E4 -> $a;
        16#EB -> $e;
        16#EF -> $i;
        16#F6 -> $o;
        16#FC -> $u;
        _ -> C
    end.

remove_accents(Word) ->
    << <<(remove_accent(C))/utf8>> || <<C/utf8>> <= Word >>.

remove_accent(C) ->
    case C of
        16#E1 -> $a;
        16#E9 -> $e;
        16#ED -> $i;
        16#F3 -> $o;
        16#FA -> $u;
        _ -> C
    end.

uppercase_i_chars(Word) ->
    Regex = [<<"([">>, ?VOWELS, <<"])i([">>, ?VOWELS, <<"])">>],
    Options = [unicode, global, {return, binary}],
    re:replace(Word, Regex, "\\1I\\2", Options).

uppercase_y_chars(Word) ->
    Word2 = case Word of
                <<"y" , Rest/bytes>> -> <<"Y", Rest/bytes>>;
                _ -> Word
            end,
    Regex = [<<"([">>, ?VOWELS, <<"])y">>],
    Options = [unicode, global, {return, binary}],
    re:replace(Word2, Regex, "\\1Y", Options).

lowercase_i_and_y_chars(Word) ->
    << <<(lowercase_i_and_y_char(C))/utf8>> || <<C/utf8>> <= Word >>.

lowercase_i_and_y_char(C) ->
    case C of
        $I -> $i;
        $Y -> $y;
        _ -> C
    end.

%%% Local variables:
%%% mode: erlang
%%% fill-column: 78
%%% coding: latin-1
%%% End:
