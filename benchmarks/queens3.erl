ret(A) -> fun (SK, FK) -> SK(A, FK) end.
draw(M, F) ->
  fun (SK, FK) ->
    M(fun (A, FK) ->
        G = F(A),
        G(SK, FK)
      end, FK)
  end.

%draw(M, F) -> fun (SK, FK) -> M(fun (A, FK) -> F(A, SK, FK) end, FK) end.

on(C, A) ->
  fun (SK, FK) ->
    if C -> SK(A, FK);
       true -> FK
    end
  end.

range(From, To) ->
  fun (SK, FK) ->
    if From > To -> FK;
       true -> F = range(From+1, To),
               SK(From, F(SK, FK))
    end
  end.

gen(0, NQ) -> ret([]);
gen(N, NQ) ->
  draw(gen(N-1, NQ),
    fun (B) ->
      draw(range(1, NQ), 
        fun (Q) ->
          on(safe(Q, 1, B), [Q|B])
        end)
    end).

safe(X, D, [Q|L]) ->
  X /= Q and
    X /= Q+D and
      X /= Q-D and
        safe(X, D+1, L);
safe(X, D, []) -> true.

start() -> 
  F = gen(12, 12),
  F(fun (X, Xs) -> 1+Xs end, 0).
