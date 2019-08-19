head([X|Xs]) -> X.
tail([X|Xs]) -> Xs.

start() -> head(tail([1,2,3])).
