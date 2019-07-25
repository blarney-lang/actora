-module(fib).
-export([main/1]).

fib(N) ->
  if
    N =< 1 -> N;
    true -> fib(N-1) + fib(N-2)
  end.

start() -> fib(40).

main(Args) ->
  Begin = os:timestamp(),
  io:format("Result: ~w\n", [start()]),
  End = os:timestamp(),
  io:format("Time: ~f\n", [timer:now_diff(End, Begin) / 1000000]).
