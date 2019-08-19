listcomp(Xs, Ys) -> [{X,Y} || X <- Xs, Y <- Ys, X < Y].

start() -> length(listcomp([1..3], [1..3])).
