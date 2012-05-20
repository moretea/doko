-module(dk_pp).
-include("../../dk_utf8/include/dk_utf8.hrl").

%% API
-export([terms/2]).

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

%% @doc Returns a list of terms.
-spec terms(utf8_string(), iso_639_1()) -> [utf8_string()].
terms(Str, Lang) ->
    [(list_to_atom("dk_stem_" ++ Lang)):stem(Token)
     || Token <- tokenize(dk_utf8:case_fold(Str), Lang),
        not(stop_word(Token, Lang))].

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

tokenize(Str, Lang) ->
    RE = case Lang of
             "en" -> <<"[a-z0-9]+">>;
             "nl" -> unicode:characters_to_binary("[a-z0-9äëïöüáéíóúè]+")
         end,
    case re:run(Str, RE, [unicode, {capture, all, binary}, global]) of
        nomatch -> [];
        {match, Tokens} -> lists:flatten(Tokens)
    end.

stop_word(Token, Lang) ->
    case Lang of
        "en" -> lists:member(Token, [
                                     <<"i">>,
                                     <<"me">>,
                                     <<"my">>,
                                     <<"myself">>,
                                     <<"we">>,
                                     <<"our">>,
                                     <<"ours">>,
                                     <<"ourselves">>,
                                     <<"you">>,
                                     <<"your">>,
                                     <<"yours">>,
                                     <<"yourself">>,
                                     <<"yourselves">>,
                                     <<"he">>,
                                     <<"him">>,
                                     <<"his">>,
                                     <<"himself">>,
                                     <<"she">>,
                                     <<"her">>,
                                     <<"hers">>,
                                     <<"herself">>,
                                     <<"it">>,
                                     <<"its">>,
                                     <<"itself">>,
                                     <<"they">>,
                                     <<"them">>,
                                     <<"their">>,
                                     <<"theirs">>,
                                     <<"themselves">>,
                                     <<"what">>,
                                     <<"which">>,
                                     <<"who">>,
                                     <<"whom">>,
                                     <<"this">>,
                                     <<"that">>,
                                     <<"these">>,
                                     <<"those">>,
                                     <<"am">>,
                                     <<"is">>,
                                     <<"are">>,
                                     <<"was">>,
                                     <<"were">>,
                                     <<"be">>,
                                     <<"been">>,
                                     <<"being">>,
                                     <<"have">>,
                                     <<"has">>,
                                     <<"had">>,
                                     <<"having">>,
                                     <<"do">>,
                                     <<"does">>,
                                     <<"did">>,
                                     <<"doing">>,
                                     <<"would">>,
                                     <<"should">>,
                                     <<"could">>,
                                     <<"ought">>,
                                     <<"cannot">>,
                                     <<"a">>,
                                     <<"an">>,
                                     <<"the">>,
                                     <<"and">>,
                                     <<"but">>,
                                     <<"if">>,
                                     <<"or">>,
                                     <<"because">>,
                                     <<"as">>,
                                     <<"until">>,
                                     <<"while">>,
                                     <<"of">>,
                                     <<"at">>,
                                     <<"by">>,
                                     <<"for">>,
                                     <<"with">>,
                                     <<"about">>,
                                     <<"against">>,
                                     <<"between">>,
                                     <<"into">>,
                                     <<"through">>,
                                     <<"during">>,
                                     <<"before">>,
                                     <<"after">>,
                                     <<"above">>,
                                     <<"below">>,
                                     <<"to">>,
                                     <<"from">>,
                                     <<"up">>,
                                     <<"down">>,
                                     <<"in">>,
                                     <<"out">>,
                                     <<"on">>,
                                     <<"off">>,
                                     <<"over">>,
                                     <<"under">>,
                                     <<"again">>,
                                     <<"further">>,
                                     <<"then">>,
                                     <<"once">>,
                                     <<"here">>,
                                     <<"there">>,
                                     <<"when">>,
                                     <<"where">>,
                                     <<"why">>,
                                     <<"how">>,
                                     <<"all">>,
                                     <<"any">>,
                                     <<"both">>,
                                     <<"each">>,
                                     <<"few">>,
                                     <<"more">>,
                                     <<"most">>,
                                     <<"other">>,
                                     <<"some">>,
                                     <<"such">>,
                                     <<"no">>,
                                     <<"nor">>,
                                     <<"not">>,
                                     <<"only">>,
                                     <<"own">>,
                                     <<"same">>,
                                     <<"so">>,
                                     <<"than">>,
                                     <<"too">>,
                                     <<"very">>
                                    ]);
        _ -> false
    end.

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
