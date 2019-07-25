-module(adjoxo).
-export([main/1]).

% Adjudicator for naughts and crosses

map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F,Xs)].

foldr1(F, [X|Xs]) ->
  case Xs of
    [] -> X;
    Other -> F(X, foldr1(F, Xs))
  end.

bestOf(win, V) -> win;
bestOf(loss, V) -> V;
bestOf(draw, V) ->
  case V of
    win -> win;
    draw -> draw;
    loss -> draw
  end.

inverse(loss) -> win;
inverse(draw) -> draw;
inverse(win) -> loss.

insert(X, []) -> [X];
insert(X, [Y|Ys]) ->
  if X =< Y -> [X|[Y|Ys]];
     true -> [Y|insert(X,Ys)]
  end.

diff([], Ys) -> [];
diff([X|Xs], YYs) ->
  case YYs of
    [] -> [X|Xs];
    [Y|Ys] ->
      if X < Y -> [X|diff(Xs, YYs)];
         X > Y -> diff([X|Xs], Ys);
         true -> diff(Xs, Ys)
      end
  end.

subset(Xs, Ys) -> diff(Xs, Ys) == [].

hasLine(P) ->
  subset([1,2,3], P) or subset([4,5,6], P) or
    subset([7,8,9], P) or subset([1,4,7], P) or
      subset([2,5,8], P) or subset([3,6,9], P) or
        subset([1,5,9], P) or subset([3,5,7], P).

gridFull(AP, PP) -> (length(AP) + length(PP)) == 9.

analysis(AP, PP) ->
  HasLine = hasLine(PP),
  GridFull = gridFull(AP, PP),
  if HasLine -> loss;
     GridFull -> draw;
     true -> foldr1(fun bestOf/2, map(moveval(AP, PP),
               diff(diff(lists:seq(1,9), AP), PP)))
  end.

moveval(AP, PP) ->
  fun (M) -> inverse(analysis(PP, insert(M, AP))) end.

adjudicate(Os, Xs) ->
  LenOs = length(Os),
  LenXs = length(Xs),
  HasLineXs = hasLine(Xs),
  HasLineOs = hasLine(Os),
  if LenOs < LenXs -> report(analysis(Xs, Os), o);
     LenOs > LenXs -> report(analysis(Xs, Os), x);
     HasLineXs -> report(win, x);
     HasLineOs -> report(win, o);
     true -> report(analysis(Xs, Os), x)
  end.

report(loss, S) -> opp(S);
report(win, S) -> S;
report(draw, P) -> draw.

opp(o) -> x;
opp(x) -> o.

start() -> adjudicate([], []).

main(Args) ->
  Begin = os:timestamp(),
  io:format("Result: ~w\n", [start()]),
  End = os:timestamp(),
  io:format("Time: ~f\n", [timer:now_diff(End, Begin) / 1000000]).
