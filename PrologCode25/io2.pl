getList(OldList, NewList) :-
    write('enter item of list'), nl, 
    read(Item),
    append ([Item], OldList, NewList),
    write('are their more symptoms? y or n '),
    read('Answer'),
    Answer =:= 'y', getList(OldList, NewList).


/* getList([], L).*/
