uncurry(F, {X,Y}) -> F(X,Y).

pair(X,Y) -> {X,Y}.

start() -> uncurry(pair, {1,2}).
