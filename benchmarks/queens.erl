start() -> length(nqueens(12)).

nqueens(NQ) -> gen(NQ, NQ).

gen(0, NQ) -> [[]];
gen(N, NQ) -> [[Q|B] || B <- gen(N-1, NQ), Q <- [1..NQ], safe(Q, 1, B)].

safe(X, D, [Q|L]) ->
  X /= Q and
    X /= Q+D and
      X /= Q-D and
        safe(X, D+1, L);
safe(X, D, []) -> true.
