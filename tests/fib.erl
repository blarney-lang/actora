fib(N) ->
  if
    N <= 1 -> N;
    true   -> fib(N-1) + fib(N-2)
  end.

start() -> fib(10);
