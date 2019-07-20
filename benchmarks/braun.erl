insert(X, empty) -> {branch, X, empty, empty};
insert(X, {branch, Y, T0, T1}) ->
  {branch, X, insert(Y, T1), T0}.

fromList([]) -> empty;
fromList([X|Xs]) -> insert(X, fromList(Xs)).

toList(empty) -> [];
toList({branch, X, T0, T1}) ->
  [X|ilv(toList(T0), toList(T1))].

ilv([], Ys) -> Ys;
ilv([X|Xs], YYs) ->
  case YYs of
    [] -> [X|Xs];
    [Y|Ys] -> [X|[Y|ilv(Xs, Ys)]]
  end.

equal([], []) -> true;
equal([], Ys) -> false;
equal([X|Xs], YYs) ->
  case YYs of
    [] -> false;
    [Y|Ys] -> (X == Y) and equal(Xs, Ys)
  end.

propConvert(Xs) -> equal(Xs, toList(fromList(Xs))).

benchmark(0, Xs) -> true;
benchmark(N, Xs) -> propConvert(Xs) and benchmark(N-1, Xs).

start() -> benchmark(10000, [1..100]).
