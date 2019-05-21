map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F, Xs)].

inv(true) -> false;
inv(false) -> true.

start() -> map(inv, [false, true, false]).
