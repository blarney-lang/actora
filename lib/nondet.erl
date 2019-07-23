% Success/fail continuation monad

% Monadic operators
return(A) -> fun (SK, FK) -> SK(A, FK) end.
mbind(M, F) ->
  fun (SK, FK) ->
    M(fun (A, FK) ->
        G = F(A),
        G(SK, FK)
      end, FK)
  end.
mfail() -> fun (SK, FK) -> FK end.

% Non-deterministic choice
mplus(L, R) ->
  fun (SK, FK) -> L(SK, R(SK, FK)) end.

% Count number of solutions
count(M) -> M(fun (X, Xs) -> 1+Xs end, 0).

% Fail if condition doesn't hold
guard(B) ->
  if
    B -> return({});
    true -> mfail()
  end.

% Pick a value between From and To (inclusive)
range(From, To) ->
  if
    From > To -> mfail();
    true -> mplus(return(From), range(From+1, To))
  end.
