id(X) -> X.

singleton(X) -> [X].

fst({X,Y}) -> X.

snd({X,Y}) -> Y.

null([]) -> true;
null(Other) -> false.

head(Xs) -> [H|T] = Xs, H.

inv(false) -> true;
inv(true) -> false.

mux(C, X, Y) ->
  if
    C -> X;
    true -> Y
  end.

map(F, []) -> [];
map(F, [X|Xs]) -> [F(X)|map(F, Xs)].

map2(F, Xs) ->
  case Xs of
    [] -> [];
    [H|T] -> [F(H)|map2(F,T)]
  end.

append([X|Xs], Ys) -> [X|append(Xs, Ys)].
append([], Ys) -> Ys;

rev(Xs,Acc) ->
  case Xs of
    [] -> Acc;
    [H|T] -> rev(T,[H|Acc])
  end.

zipWith(F, Xs, Ys) ->
  case Xs of
    [] -> [];
    [HXs|TXs] ->
      case Ys of
        [] -> [];
        [HYs|TYs] -> [F(HXs,HYs)|zipWith(F,TXs,TYs)]
      end
  end.

filter(P,[]) -> [];
filter(P,[H|T]) when P(H) -> [H|filter(P,T)];
filter(P,[H|T]) -> filter(P,T).
