square( X, Y) :- Y is X * X.
maplist1( [], _, []).
maplist1( [X|Tail], F, [NewX|NewTail]) :-
G =.. [F, X, NewX], call( G), maplist1(Tail, F, NewTail).
