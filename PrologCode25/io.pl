position('Spielberg', director).
position('Allen', manager).
position('Lee', supervisor).

find_position:-
	write('Whose position do you wish to know?'),
	read(Input), 
	position(Input, Output),
	write('The position of  '), 
	write(Input),	write(' is '), write(Output), write('.').


/* querry as : ? find_position.*/
