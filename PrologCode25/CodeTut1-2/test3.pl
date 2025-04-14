%popultaion.pl Population in Million
pop(usa,280).
pop(india,1000).
pop(china,1200). pop(brazil,130).
area(usa,3). /* millions of sq miles */
area(india,1). area(china,4).
area(brazil,3).
density(X,Y) :- pop(X,P), area(X,A), Y is P/A.
