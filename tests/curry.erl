curry(F, X, Y) -> F({X,Y}).

fst({X,Y}) -> X.

start() -> 
  F = curry(fst),
  F(1,2).
