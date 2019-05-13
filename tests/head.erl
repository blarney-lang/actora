head([X|Xs]) -> X.
tail([X|Xs]) -> Xs.

start() -> {head([1,2,3]), tail([1,2,3])}.
