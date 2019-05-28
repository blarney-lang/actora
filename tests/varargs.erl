sum(Acc) ->
  fun
    (fin) -> Acc;
    (X) -> sum(X+Acc)
  end.

start() -> sum(1, 2, 3, 4, fin).
