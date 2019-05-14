length([X|Xs]) -> 1 + length(Xs);
length([]) -> 0.

start() -> length([1,2,3,4]).
