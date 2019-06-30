comp(Xs, Ys) -> [{X,Y} || X <- Xs, Y <- Ys, X < Y].

start() -> comp([1,2,3], [1,2,3]).
