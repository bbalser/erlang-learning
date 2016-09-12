-module(roman).
-export([toRoman/1, toArabic/1]).
-include_lib("eunit/include/eunit.hrl").

-record(conversion, {roman, arabic}).
-define(ROMAN_VALUES, [
                        #conversion{roman="M", arabic=1000},
                        #conversion{roman="CM", arabic=900},
                        #conversion{roman="D", arabic=500},
                        #conversion{roman="CD", arabic=400},
                        #conversion{roman="C", arabic=100},
                        #conversion{roman="XC", arabic=90},
                        #conversion{roman="L", arabic=50},
                        #conversion{roman="XL", arabic=40},
                        #conversion{roman="X", arabic=10},
                        #conversion{roman="IX", arabic=9},
                        #conversion{roman="V", arabic=5},
                        #conversion{roman="IV", arabic = 4},
                        #conversion{roman="I", arabic=1}
                      ]).

toRoman(Arabic) ->
  {_, Result} = lists:foldl(fun convertToRoman/2, {Arabic, ""}, ?ROMAN_VALUES),
  Result.

convertToRoman(Elem, {Arabic, Result}) ->
  Rem = Arabic rem Elem#conversion.arabic,
  Times = Arabic div Elem#conversion.arabic,
  {Rem, string:concat(Result, string:copies(Elem#conversion.roman, Times))}.


toArabic(Roman) ->
  {_, Result} = lists:foldl(fun convertToArabic/2, {Roman, 0}, ?ROMAN_VALUES),
  Result.

convertToArabic(Elem, {Roman, Result}) ->
  Roman_Len = string:len(Elem#conversion.roman),
  Start = string:left(Roman, Roman_Len),
  if
    Start =:= Elem#conversion.roman -> convertToArabic(Elem, {string:sub_string(Roman, Roman_Len+1), Result + Elem#conversion.arabic});
    true -> {Roman, Result}
  end.

toRoman_test_() ->
  [
    ?_assertEqual("I", toRoman(1)),
    ?_assertEqual("II", toRoman(2)),
    ?_assertEqual("III", toRoman(3)),
    ?_assertEqual("IV", toRoman(4)),
    ?_assertEqual("V", toRoman(5)),
    ?_assertEqual("VI", toRoman(6)),
    ?_assertEqual("IX", toRoman(9)),
    ?_assertEqual("X", toRoman(10)),
    ?_assertEqual("XI", toRoman(11)),
    ?_assertEqual("XX", toRoman(20)),
    ?_assertEqual("XL", toRoman(40)),
    ?_assertEqual("L", toRoman(50)),
    ?_assertEqual("XC", toRoman(90)),
    ?_assertEqual("C", toRoman(100)),
    ?_assertEqual("CD", toRoman(400)),
    ?_assertEqual("D", toRoman(500)),
    ?_assertEqual("CM", toRoman(900)),
    ?_assertEqual("M", toRoman(1000)),
    ?_assertEqual("MMMCMXCIX", toRoman(3999)),
    ?_assertEqual("MMMDCCCXXXVIII", toRoman(3838))
  ].

toArabic_test_() ->
  [
    ?_assertEqual(1, toArabic("I")),
    ?_assertEqual(2, toArabic("II")),
    ?_assertEqual(4, toArabic("IV")),
    ?_assertEqual(5, toArabic("V")),
    ?_assertEqual(6, toArabic("VI")),
    ?_assertEqual(9, toArabic("IX")),
    ?_assertEqual(10, toArabic("X")),
    ?_assertEqual(20, toArabic("XX")),
    ?_assertEqual(40, toArabic("XL")),
    ?_assertEqual(50, toArabic("L")),
    ?_assertEqual(90, toArabic("XC")),
    ?_assertEqual(100, toArabic("C")),
    ?_assertEqual(400, toArabic("CD")),
    ?_assertEqual(500, toArabic("D")),
    ?_assertEqual(900, toArabic("CM")),
    ?_assertEqual(1000, toArabic("M")),
    ?_assertEqual(3999, toArabic("MMMCMXCIX")),
    ?_assertEqual(3838, toArabic("MMMDCCCXXXVIII"))
  ].