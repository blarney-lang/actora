map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F, Xs)].

int(true) -> 1;
int(false) -> 0.

start() -> sum(map(int, [false, true, false])).
