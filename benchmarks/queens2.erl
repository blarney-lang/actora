range(From, To, SK, FK) ->
  if From > To -> FK;
     true -> SK(From, range(From+1, To, SK, FK))
  end.

gen(0, NQ, SK, FK) -> SK([], FK);
gen(N, NQ, SK, FK) ->
  gen(N-1, NQ, fun (B, FK) -> 
    range(1, NQ, fun (Q, FK) -> 
      if safe(Q, 1, B) -> SK([Q|B], FK);
         true -> FK
      end
    end, FK)
  end, FK).

safe(X, D, [Q|L]) ->
  X /= Q and
    X /= Q+D and
      X /= Q-D and
        safe(X, D+1, L);
safe(X, D, []) -> true.

%start() -> length(gen(8, 8, fun (X, Xs) -> [X|Xs] end, [])).
start() -> gen(12, 12, fun (X, Xs) -> 1+Xs end, 0).
