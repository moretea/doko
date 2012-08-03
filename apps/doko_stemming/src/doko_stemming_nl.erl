%%----------------------------------------------------------------------------
%% @doc Dutch stemming library.
%% @reference See <a
%% href="http://snowball.tartarus.org/algorithms/dutch/stemmer.html"> this
%% page</a> for more information about the algorithm used.
%%----------------------------------------------------------------------------

-module(doko_stemming_nl).

%% API
%% -export([stem/1]).
-compile(export_all).

%% Marco definitions
-define(VOWELS, <<"aeiouy", 16#C3A8:16>>).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

-spec stem(doko_utf8:str()) -> doko_utf8:str().
stem(Word)  ->
    Length = doko_utf8:length(Word),
    if
        Length =< 3 ->
            remove_umlauts(remove_accents(Word));
        true ->
            lowercase_i_and_y_chars(
              step4(step3b(step3a(step2(step1(preprocess(Word)))))))
    end.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

step1(Word) ->
    {StdR1, _R2} = standard_r1_r2(Word),
    R1 = adjust_r1(Word, Word, StdR1),
    Heden = doko_utf8:suffix(<<"heden">>, R1),
    Ene = doko_utf8:suffix(<<"ene">>, R1),
    En = doko_utf8:suffix(<<"en">>, R1),
    Se = doko_utf8:suffix(<<"se">>, R1),
    S = doko_utf8:suffix(<<"s">>, R1),
    if
        Heden ->
            <<(doko_utf8:substr(Word, 0, -5))/bytes,"heid">>;
        Ene or En ->
            WordHeden = doko_utf8:suffix(<<"heden">>, Word),
            NewWord = if
                          En  -> doko_utf8:substr(Word, 0, -2);
                          Ene -> doko_utf8:substr(Word, 0, -3)
                      end,
            ValidEnding = valid_en_ending(NewWord),
            if
                WordHeden or not ValidEnding -> Word;
                true                         -> undouble_end(NewWord)
            end;
        Se or S ->
            NewWord = if
                          S  -> doko_utf8:substr( Word, 0, -1);
                          Se -> doko_utf8:substr( Word, 0, -2)
                      end,
            ValidEnding = valid_s_ending(NewWord),
            if
                ValidEnding -> NewWord;
                true        -> Word
            end;
        true ->
            Word
    end.

step2(Word) ->
    {StdR1,_R2} = standard_r1_r2(Word),
    R1 = adjust_r1(Word, Word, StdR1),
    SuffixE = doko_utf8:suffix(<<"e">>, R1),
    if
        not SuffixE ->
            {Word,false};
        true ->
            Regex = [<<"[^">>,?VOWELS,<<"]e$">>],
            Options = [unicode],
            case re:run(Word, Regex, Options) of
                nomatch ->
                    {Word,false};
                _ ->
                    NewWord = undouble_end(doko_utf8:substr(Word, 0, -1)),
                    {NewWord,true}
            end
    end.

step3a({Word,Step2Success}) ->
    {StdR1,R2} = standard_r1_r2(Word),
    R1 = adjust_r1(Word, Word, StdR1),
    Heid = doko_utf8:suffix(<<"heid">>, R2),
    Cheid = doko_utf8:suffix(<<"cheid">>, Word),
    if
        (not Heid) or Cheid ->
            {Word, Step2Success};
        true ->
            NewWord = doko_utf8:substr(Word, 0, -4),
            NewR1 = doko_utf8:substr(R1, 0, -4),
            En = doko_utf8:suffix(<<"en">>, NewR1),
            if
                not En ->
                    {NewWord,Step2Success};
                true ->
                     ValidEnding =
                        valid_en_ending(doko_utf8:substr(NewR1, 0, -2)),
                    if
                        not ValidEnding ->
                            {NewWord,Step2Success};
                        true ->
                            NewNewWord = undouble_end(
                                           doko_utf8:substr(NewWord, 0, -2)),
                            {NewNewWord,Step2Success}
                    end
            end
    end.

step3b({Word, Step2Success}) ->
    {_StdR1, R2} = standard_r1_r2(Word),
    End = doko_utf8:suffix(<<"end">>, R2),
    Ing = doko_utf8:suffix(<<"ing">>, R2),
    Ig = doko_utf8:suffix(<<"ig">>, R2),
    Eig = doko_utf8:suffix(<<"eig">>, Word),
    Lijk = doko_utf8:suffix(<<"lijk">>, R2),
    Baar = doko_utf8:suffix(<<"baar">>, R2),
    Bar = doko_utf8:suffix(<<"bar">>, R2),
    if
        End or Ing ->
            NewWord = doko_utf8:substr(Word, 0, -3),
            NewR2 = doko_utf8:substr(R2, 0, -3),
            Ig2 = doko_utf8:suffix(<<"ig">>, NewR2),
            Eig2 = doko_utf8:suffix(<<"eig">>, NewWord),
            if
                Ig2 and not Eig2 ->
                    doko_utf8:substr(NewWord, 0, -2);
                true ->
                    undouble_end(NewWord)
            end;
        Ig and not Eig ->
            doko_utf8:substr(Word, 0, -2);
        Lijk ->
            {NewWord, _} = step2(doko_utf8:substr(Word, 0, -4)),
            NewWord;
        Baar ->
            doko_utf8:substr(Word, 0, -4);
        Bar and Step2Success ->
            doko_utf8:substr(Word, 0, -3);
        true ->
            Word
    end.

step4(Word) ->
    Regex = [<<"^(.*)([^">>,?VOWELS,<<"])(aa|ee|oo|uu)">>,
             <<"([^I">>,?VOWELS,<<"])$">>],
    Options = [unicode,global,{capture,all_but_first,binary}],
    case re:run(Word, Regex, Options) of
        {match, [[Begin,C,<<V/utf8,_>>, D]]} ->
            <<Begin/bytes,C/bytes,V,D/bytes>>;
        nomatch ->
            Word
    end.

standard_r1_r2(Word) ->
    Regex = [<<".*?[">>,?VOWELS,<<"][^">>,?VOWELS,<<"](.*)$">>],
    Options = [unicode,global,{capture,all_but_first,binary}],
    R1 = case re:run(Word, Regex, Options) of
             {match,[[Match1]]} -> Match1;
             nomatch            -> <<>>
         end,
    R2 = case re:run(R1, Regex, Options) of
             {match,[[Match2]]} -> Match2;
             nomatch            -> <<>>
         end,
    {R1, R2}.

adjust_r1(Word, <<V/utf8,C/utf8,L1/utf8,L2/utf8>>, R1) ->
    Test = vowel(V) and not vowel(C),
    case Test of
        true  -> doko_utf8:substr(Word, 3);
        false -> adjust_r1(Word, <<C/utf8,L1/utf8,L2/utf8>>, R1)
    end;
adjust_r1(Word, <<V/utf8,C/utf8,_/utf8>>, R1) ->
    Test = vowel(V) and not vowel(C),
    case Test of
        true  -> doko_utf8:substr(Word, 3);
        false -> R1
    end;
adjust_r1(Word, <<_/utf8,Rest/bytes>>, R1) ->
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
       _  -> false
    end.

valid_s_ending(Word) ->
    Drow = doko_utf8:reverse(Word),
    case Drow of
        <<"j"/utf8,_/bytes>> -> false;
        <<C/utf8,_/bytes>>   -> not vowel(C)
    end.

valid_en_ending(<<>>) ->
    false;
valid_en_ending(Word) ->
    Drow = doko_utf8:reverse(Word),
    case Drow of
        <<"meg",_/bytes>>  -> false;
        <<C/utf8,_/bytes>> -> not vowel(C)
    end.

undouble_end(Word) ->
    DoubleEnd = doko_utf8:suffix(<<"kk">>, Word) or
                doko_utf8:suffix(<<"dd">>, Word) or
                doko_utf8:suffix(<<"tt">>, Word),
    if
        DoubleEnd -> doko_utf8:substr(Word, 0, -1);
        true      -> Word
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
        _     -> C
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
        _     -> C
    end.

uppercase_i_chars(Word) ->
    Regex = [<<"([">>,?VOWELS,<<"])i([">>,?VOWELS,<<"])">>],
    Options = [unicode,global,{return,binary}],
    re:replace(Word, Regex, "\\1I\\2", Options).

uppercase_y_chars(Word) ->
    Word2 = case Word of
                <<"y",Rest/bytes>> -> <<"Y",Rest/bytes>>;
                _                  -> Word
            end,
    Regex = [<<"([">>,?VOWELS,<<"])y">>],
    Options = [unicode,global,{return,binary}],
    re:replace(Word2, Regex, "\\1Y", Options).

lowercase_i_and_y_chars(Word) ->
    << <<(lowercase_i_and_y_char(C))/utf8>> || <<C/utf8>> <= Word >>.

lowercase_i_and_y_char(C) ->
    case C of
        $I -> $i;
        $Y -> $y;
        _  -> C
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
