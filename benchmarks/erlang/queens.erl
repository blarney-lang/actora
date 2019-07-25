% Solution to N queens problem using success/fail continuations

-module(queens).
-export([main/1]).

range(From, To, SK, FK) ->
  if From > To -> FK;
     true -> SK(From, range(From+1, To, SK, FK))
  end.

gen(0, NQ, SK, FK) -> SK([], FK);
gen(N, NQ, SK, FK) ->
  gen(N-1, NQ, fun (B, FK) -> 
    range(1, NQ, fun (Q, FK) -> 
      Safe = safe(Q, 1, B),
      if Safe -> SK([Q|B], FK);
         true -> FK
      end
    end, FK)
  end, FK).

safe(X, D, [Q|L]) ->
  X /= Q andalso
    X /= Q+D andalso
      X /= Q-D andalso
        safe(X, D+1, L);
safe(X, D, []) -> true.

start() -> gen(12, 12, fun (X, Xs) -> 1+Xs end, 0).

main(Args) ->
  Begin = os:timestamp(),
  io:format("Result: ~w\n", [start()]),
  End = os:timestamp(),
  io:format("Time: ~f\n", [timer:now_diff(End, Begin) / 1000000]).
