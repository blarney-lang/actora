-import(nondet).

gen(0, NQ) -> return([]);
gen(N, NQ) ->
  do
    B <- gen(N-1, NQ),
    Q <- range(1, NQ),
    guard(safe(Q, 1, B)),
    return([Q|B])
  end.

safe(X, D, [Q|L]) ->
  X /= Q and
    X /= Q+D and
      X /= Q-D and
        safe(X, D+1, L);
safe(X, D, []) -> true.

start() -> count(gen(12, 12)).
