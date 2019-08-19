% Simple list monad
return(X) -> [X].
mbind(M, F) -> concatMap(F, M).
mfail() -> [].

start() ->
  length(
    do
      X <- [1..3],
      Y <- [1..3],
      return({X,Y})
    end).
