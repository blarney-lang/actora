tri(N, Acc) ->
  if
    N == 1 -> Acc;
    true -> tri(N-1, Acc+N)
  end.

start() -> tri(10, 1).
