country(argentina). country(bolivia). country(brazil).
country(columbia). country(chile). country(paraguay). country(peru).
country(uruguay). country(venezuela).

beside(argentina,bolivia). beside(argentina,brazil).
beside(argentina,chile). beside(argentina,paraguay).
beside(argentina,uruguay). beside(bolivia,brazil).
beside(bolivia,chile). beside(bolivia,paraguay).
beside(bolivia,peru). beside(brazil,columbia).
beside(brazil,paraguay). beside(brazil,peru).
beside(brazil,uruguay). beside(brazil,venezuela).
beside(chile,peru). beside(columbia,peru).
beside(columbia,venezuela). beside(guyana,venezuela).

borders(Country,Neighbor) :- beside(Country,Neighbor). 
borders(Country,Neighbor) :- beside(Neighbor,Country).

color(red).
color(green).
color(black).
color(yellow).
color(gray).

prohibited(Country,Hue,Sofar) :- borders(Country,Neighbor), member([Neighbor,Hue],Sofar).

color_map(Sofar,Solution) :- 
		country(Country), 
		\+ member([Country,_],Sofar),
		color(Hue),
		\+ prohibited(Country,Hue,Sofar),
		color_map([[Country,Hue]|Sofar],Solution).

/*color_map([],Solution).*/

