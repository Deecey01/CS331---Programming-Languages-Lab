connect(start,2).
connect(3,4).
connect(2,3).
connect(5,11).
connect(8,9).
connect(12,18).
connect(14,20).
connect(17,23).
connect(20,26).
connect(24,30).
connect(27,28).
connect(30,36).
connect(33,34).
connect(32,finish).
connect(1,7).
connect(3,9).
connect(5,6).
connect(10,16).
connect(13,14).
connect(15,21).
connect(18,24).
connect(21,22).
connect(25,31).
connect(28,29).
connect(31,32).
connect(34,35).
connect(2,8).
connect(4,10).
connect(7,13).
connect(11,17).
connect(14,15).
connect(16,22).
connect(19,25).
connect(23,29).
connect(26,27).
connect(28,34).
connect(32,33).
connect(35,36).


con_sym(Locx,Locy) :- connect(Locx,Locy).
con_sym(Locx,Locy) :- connect(Locy,Locx).

path([finish|RestOfPath],[finish|RestOfPath]).

path([CurrLoc|RestOfPath],Solution) :- con_sym(CurrLoc,NextLoc), 
		\+ member(NextLoc,RestOfPath), 
		path([NextLoc,CurrLoc|RestOfPath],Solution).

solve_maze(X,Solution):- path([X],Solution).

printlist([]).
printlist([X|List]) :- write(X),nl, printlist(List).


/*   Do querry : solve_maze(start,Solution). */

