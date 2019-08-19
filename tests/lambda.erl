map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F, Xs)].

start() ->
  F = fun
        (true) -> 1;
        (false) -> 0
      end,
  sum(map(F, [false, true, false])).
