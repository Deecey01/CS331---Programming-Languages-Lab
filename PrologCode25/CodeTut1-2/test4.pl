border(cg,wb).
border(cg,br).
border(br,wb).
border(mp,cg).
border(mp,br).
border(mp,up).
border(up,br).
border(rj,mp).
border(rj,up).

adjacent(X,Y):-border(X,Y).
adjacent(X,Y):-border(Y,X).

/*border(X,Y):-border(Y,X).*/

valid(X,Y):-adjacent(X,Z),adjacent(Z,Y).



