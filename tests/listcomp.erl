listcomp(Xs, Ys) -> [{X,Y} || X <- Xs, Y <- Ys, X < Y].

start() -> listcomp([1..3], [1..3]).
