abstract(0) -> 1234;
abstract(N) -> fun (X) -> abstract(N-1) end.

app(F, 0) -> F;
app(F, N) -> app(F(0), N-1).

start() ->
  F = abstract(64),
  app(F, 64).
