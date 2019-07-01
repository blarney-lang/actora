append([X|Xs], Ys) -> [X|append(Xs,Ys)];
append([], Ys) -> Ys.

concatMap(F, [X|Xs]) -> F(X) ++ concatMap(F, Xs);
concatMap(F, []) -> [].

lengthPlus([X|Xs], Acc) -> lengthPlus(Xs, 1+Acc);
lengthPlus([], Acc) -> Acc.

length(Xs) -> lengthPlus(Xs, 0).

enumFromTo(From, To) when From > To -> [];
enumFromTo(From, To) -> [From|enumFromTo(From+1, To)].
