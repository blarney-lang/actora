id(X) -> X.

compose(F,G) -> fun (X) -> F(G(X)) end.

foldr(F, Z, []) -> Z;
foldr(F, Z, [X|Xs]) -> F(X, foldr(F, Z, Xs)).

inc(X) -> X+1.

start() ->
  F = foldr(compose, id, [inc, inc, inc]),
  F(0).
