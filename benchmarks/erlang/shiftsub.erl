% Division by shift-and-subtract
% (Should favour a register machine: tight tail-recursive loo
% with plenty of primitives and temporaries)

-module(shiftsub).
-export([main/1]).

sas(0, N, D, R, Q) -> Q;
sas(I, N, D, R, Q) ->
  J = I-1,
  NewR = (R bsl 1) bor ((N bsr J) band 1),
  if
    NewR >= D -> sas(J, N, D, NewR-D, Q bor (1 bsl J));
    true -> sas(J, N, D, NewR, Q)
  end.

benchmark(0, Acc) -> Acc;
benchmark(N, Acc) ->
  benchmark(N-1, sas(32, 60001, 13, 0, 0)).

start() -> benchmark(1000000, 0).

main(Args) ->
  Begin = os:timestamp(),
  io:format("Result: ~w\n", [start()]),
  End = os:timestamp(),
  io:format("Time: ~f\n", [timer:now_diff(End, Begin) / 1000000]).
