-module(babysitter).
-export([calc/3]).
-include_lib("eunit/include/eunit.hrl").

-define(BEFORE_BEDTIME, 10).
-define(AFTER_BEDTIME, 6).
-define(AFTER_MIDNIGHT, 15).

calc(Start,End,Bedtime) ->
      Range = lists:seq(mapTime(Start), (mapTime(End)-1)),
      MapToRate = fun(X) -> rate(X, Bedtime) end,
      lists:sum(lists:map(MapToRate, Range)).

mapTime(Time) when Time < 5 -> Time + 12;
mapTime(Time) -> Time.

rate(Hour, _) when Hour >= 12 ->
      ?AFTER_MIDNIGHT;
rate(Hour, Bedtime) when Hour >= Bedtime ->
      ?AFTER_BEDTIME;
rate(_, _) ->
      ?BEFORE_BEDTIME.

babysitter_test_() ->
  [
    ?_assertEqual(10, calc(5,6,8)),
    ?_assertEqual(6, calc(8, 9, 8)),
    ?_assertEqual(15, calc(1, 2, 8)),
    ?_assertEqual(20, calc(5,7,8)),
    ?_assertEqual(15, calc(12, 1, 8)),
    ?_assertEqual(12, calc(9, 11, 8)),
    ?_assertEqual(16, calc(7, 9, 8))
  ].
