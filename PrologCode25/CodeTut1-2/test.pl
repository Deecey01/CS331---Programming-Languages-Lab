append([],Y,Y).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
