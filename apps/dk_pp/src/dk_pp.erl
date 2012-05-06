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
    lists:map(
      fun (T) -> apply(list_to_atom("dk_stem_" ++ Lang), stem, [T]) end, 
      tokenize(dk_utf8:case_fold(Str), Lang)).

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

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
