map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F, Xs)].

inv(true) -> false.
inv(false) -> true.

start() ->
  F = fun
        (true) -> false;
        (false) -> true
      end,
  map(F, [false, true, false]).
