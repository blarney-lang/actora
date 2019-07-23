-import(nondet).

start() ->
  count (
    do
      X <- range(1,3),
      Y <- range(1,4),
      return({X,Y})
    end
  ).
