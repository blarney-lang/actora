append([X|Xs], Ys) -> [X|append(Xs,Ys)];
append([], Ys) -> Ys.

concatMap(F, [X|Xs]) -> append(F(X), concatMap(F, Xs));
concatMap(F, []) -> [].
