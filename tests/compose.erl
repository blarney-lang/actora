id(X) -> X.

compose(F,G,X) -> F(G(X)).

foldr(F, Z, []) -> Z;
foldr(F, Z, [X|Xs]) -> F(X, foldr(F, Z, Xs)).

inc(X) -> X+1.

start() -> foldr(compose, id, [inc, inc, inc], 0).
