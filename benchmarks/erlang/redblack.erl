% From "Red-Black Trees in a Functional Setting" by Okasaki.

-module(redblack).
-export([main/1]).

member(X, e) -> false;
member(X, {Col, A, Y, B}) ->
  if
    X < Y -> member(X, A);
    X == Y -> true;
    true -> member(X, B)
  end.

insert(X, S) -> makeBlack(ins(X, S)).

makeBlack({Col, A, Y, B}) -> {b, A, Y, B}.

ins(X, e) -> {r, e, X, e};
ins(X, {Col, A, Y, B}) ->
  if
    X < Y -> balance(Col, ins(X, A), Y, B);
    X == Y -> {Col, A, Y, B};
    true -> balance(Col, A, Y, ins(X, B))
  end.

balance(b, {r,{r,A,X,B},Y,C}, Z, D) -> {r,{b,A,X,B},Y,{b,C,Z,D}};
balance(b, {r,A,X,{r,B,Y,C}}, Z, D) -> {r,{b,A,X,B},Y,{b,C,Z,D}};
balance(b, A, X, {r,{r,B,Y,C},Z,D}) -> {r,{b,A,X,B},Y,{b,C,Z,D}};
balance(b, A, X, {r,B,Y,{r,C,Z,D}}) -> {r,{b,A,X,B},Y,{b,C,Z,D}};
balance(Col, A, X, B) -> {Col, A, X, B}.

build(0) -> e;
build(N) -> insert(N, build(N-1)).

check(0, T) -> true;
check(N, T) -> member(N, T) and check(N-1, T).

buildAndCheck(N) -> check(N, build(N)).

benchmark(0, N) -> true;
benchmark(M, N) -> buildAndCheck(N) and benchmark(M-1, N).

start() -> benchmark(2000, 500).

main(Args) ->
  Begin = os:timestamp(),
  io:format("Result: ~w\n", [start()]),
  End = os:timestamp(),
  io:format("Time: ~f\n", [timer:now_diff(End, Begin) / 1000000]).
