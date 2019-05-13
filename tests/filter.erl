filter(P,[]) -> [];
filter(P,[H|T]) when P(H) -> [H|filter(P,T)];
filter(P,[H|T]) -> filter(P,T).

null([]) -> true.
null(Other) -> false.

start() -> filter(null, [[1,2], [], [3,4]]).
