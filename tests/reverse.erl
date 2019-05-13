rev(Xs,Acc) ->
  case Xs of
    [] -> Acc;
    [H|T] -> rev(T,[H|Acc])
  end.

reverse(Xs) -> rev(Xs, []).

start() -> reverse([1,2,3]).
