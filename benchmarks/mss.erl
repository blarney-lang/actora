% Compute maximum segment sum

init(Pre, [], FK, SK) -> SK(Pre, FK);
init(Pre, [X|Xs], FK, SK) ->
  NewFK = if null(Pre) -> FK; true -> SK(Pre, FK) end,
  init([X|Pre], Xs, NewFK, SK).

segment(Xs, FK, SK) ->
  init([], Xs, FK, fun (I, FK) ->
    init([], I, FK, SK) end).

mss(Xs) ->
  segment(Xs, 0,
    fun (S, Max) ->
      Sum = sum(S),
      if Sum > Max -> Sum; true -> Max end
    end).

benchmark(0, Xs) -> 0;
benchmark(N, Xs) -> mss(Xs) + benchmark(N-1, Xs).

start() -> benchmark(5, [1..500]).
