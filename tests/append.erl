append([X|Xs], Ys) -> [X|append(Xs, Ys)];
append([], Ys) -> Ys.

start() -> append([1,2,3], [4,5]).
