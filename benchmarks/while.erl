% Semantics of While language, with naive While program to count divisors

lookup([{X,Y}|S], V) ->
  if X == V -> Y;
     true -> lookup(S, V)
  end.

update([{X,Y}|S], V, W) ->
  if X == V -> [{X,W}|S];
     true -> [{X,Y}|update(S, V, W)]
  end.

aval({int, N}, S) -> N;
aval({var, X}, S) -> lookup(S, X);
aval({add, A1, A2}, S) -> aval(A1, S) + aval(A2, S);
aval({sub, A1, A2}, S) -> aval(A1, S) - aval(A2, S);

bval(true, S) -> true;
bval(false, S) -> false;
bval({eq, A1, A2}, S) -> aval(A1, S) == aval(A2, S);
bval({leq, A1, A2}, S) -> aval(A1, S) <= aval(A2, S);
bval({neg, B}, S) -> not(bval(B, S));
bval({con, B1, B2}, S) -> bval(B1, S) and bval(B2, S).

stm(skip, S) -> {final, S};
stm({ass, X, A}, S) -> {final, update(S, X, aval(A, S))};
stm({comp, S1, S2}, S) ->
  case stm(S1, S) of
    {inter, NewS1, NewS} -> {inter, {comp, NewS1, S2}, NewS};
    {final, NewS} -> {inter, S2, NewS}
  end;
stm({iff, B, S1, S2}, S) ->
  if bval(B, S) -> {inter, S1, S};
     true -> {inter, S2, S}
  end;
stm({while, B, S1}, S) ->
  {inter, {iff, B, {comp, S1, {while, B, S1}}, skip}, S}.

run({inter, S1, S}) -> run(stm(S1, S));
run({final, S}) -> S.

sos(S1, S) -> run({inter, S1, S}).

start() ->
  Divide = {while, {leq, {var, 1}, {var, 0}},
              {comp, {ass, 0, {sub, {var, 0}, {var, 1}}},
                     {ass, 2, {add, {var, 2}, {int, 1}}}}},
  CallDivide = {comp, {comp, {ass, 0, {var, 3}},
                             {ass, 1, {var, 4}}}, Divide},
  NDivs = {comp, {ass, 4, {var, 3}},
             {while, {neg, {eq, {var, 4}, {int, 0}}},
                     {comp, {comp, CallDivide,
                                   {iff, {eq, {var, 0}, {int, 0}},
                                         {ass, 5, {add, {var, 5}, {int, 1}}},
                                         skip}},
                            {ass, 4, {sub, {var, 4}, {int, 1}}}}}},
  S = [{0,0}, {1,0}, {2,0}, {3, 14000}, {4, 0}, {5, 0}],
  lookup(sos(NDivs, S), 5).
