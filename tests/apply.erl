apply(F, X) -> F(X).

inc(X) -> X+1.

start() -> apply(inc, 1).
