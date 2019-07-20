append([X|Xs], Ys) -> [X|append(Xs,Ys)];
append([], Ys) -> Ys.

concatMap(F, [X|Xs]) -> F(X) ++ concatMap(F, Xs);
concatMap(F, []) -> [].

map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F,Xs)].

lengthPlus([X|Xs], Acc) -> lengthPlus(Xs, 1+Acc);
lengthPlus([], Acc) -> Acc.

length(Xs) -> lengthPlus(Xs, 0).

enumFromTo(From, To) when From > To -> [];
enumFromTo(From, To) -> [From|enumFromTo(From+1, To)].

foldr(F, Z, []) -> Z;
foldr(F, Z, [X|Xs]) -> F(X, foldr(F, Z, Xs)).

foldr1(F, [X|Xs]) ->
  case Xs of
    [] -> X;
    Other -> F(X, foldr1(F, Xs))
  end.

null([]) -> true;
null(Other) -> false.
