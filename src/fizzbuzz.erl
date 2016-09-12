-module(fizzbuzz).
-export([process/1]).
-include_lib("eunit/include/eunit.hrl").

process(List) ->
    process(List, []).

process([], Result) ->
    Result;
process([Head | Tail], Result) when Head rem 15 == 0 ->
    process(Tail, Result ++ ["fizzbuzz"]);
process([Head | Tail], Result) when Head rem 3 == 0 ->
    process(Tail, Result ++ ["fizz"]);
process([Head | Tail], Result) when Head rem 5 == 0 ->
    process(Tail, Result ++ ["buzz"]);
process([Head | Tail], Result) ->
    process(Tail, Result ++ [integer_to_list(Head)]).

fizzbuzz_test_() ->
  [
    ?_assertEqual(["1"], process([1])),
    ?_assertEqual(["1","2"], process([1,2])),
    ?_assertEqual(["fizz"], process([3])),
    ?_assertEqual(["buzz"], process([5])),
    ?_assertEqual(["fizzbuzz"], process([15])),
    ?_assertEqual(["fizz","fizz","fizz"], process([3,6,9])),
    ?_assertEqual(["buzz","buzz","buzz"], process([5, 10, 20]))
  ].
