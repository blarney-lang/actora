% Compute maximum segment sum

-module(mss).
-export([main/1]).

init(Pre, [], FK, SK) -> SK(Pre, FK);
init(Pre, [X|Xs], FK, SK) ->
  Null = Pre == [],
  NewFK = if Null -> FK; true -> SK(Pre, FK) end,
  init([X|Pre], Xs, NewFK, SK).

segment(Xs, FK, SK) ->
  init([], Xs, FK, fun (I, FK) ->
    init([], I, FK, SK) end).

mss(Xs) ->
  segment(Xs, 0,
    fun (S, Max) ->
      Sum = lists:sum(S),
      if Sum > Max -> Sum; true -> Max end
    end).

benchmark(0, Xs) -> 0;
benchmark(N, Xs) -> mss(Xs) + benchmark(N-1, Xs).

start() -> benchmark(5, lists:seq(1,500)).

main(Args) ->
  Begin = os:timestamp(),
  io:format("Result: ~w\n", [start()]),
  End = os:timestamp(),
  io:format("Time: ~f\n", [timer:now_diff(End, Begin) / 1000000]).
