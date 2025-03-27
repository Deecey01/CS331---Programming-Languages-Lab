% CS331 Assignment V

% Task 1: Duplicate Detection
% Predicate to check if a list has duplicates
has_duplicates(X) :- 
    member(Element, X),
    select(Element, X, Rest),
    member(Element, Rest).

% Interactive duplicate checking program
duplicate_checker :-
    write('Welcome to Duplicate Checker!'), nl,
    write('Enter a list of elements (use square brackets, comma-separated).'), nl,
    write('Example: [a,b,c,d,a]'), nl,
    read(List),
    (has_duplicates(List) ->
        write('The list contains duplicates!'), nl
    ;
        write('No duplicates found in the list.'), nl
    ),
    write('Do you want to check another list? (yes/no)'), nl,
    read(Continue),
    (Continue == yes -> duplicate_checker ; true).

% Task 2: Square Root Finder
% Predicate to find square root with given accuracy
squareroot(X, Result, Accuracy) :-
    X > 0,  % ensure positive number
    Initial is X / 3,  % initial guess
    sqrt_recursor(X, Initial, Accuracy, Result).

% Helper predicate to iteratively improve the square root approximation
sqrt_recursor(X, Guess, Accuracy, Result) :-
    abs(Guess * Guess - X) =< Accuracy, 
    Result = Guess.
sqrt_recursor(X, Guess, Accuracy, Result) :-
    abs(Guess * Guess - X) > Accuracy,
    NewGuess is (Guess + X / Guess) / 2,
    sqrt_recursor(X, NewGuess, Accuracy, Result).

% Interactive square root finder program
square_root_finder :-
    write('Welcome to Square Root Finder!'), nl,
    write('Enter a positive number:'), nl,
    read(Number),
    (Number > 0 ->
        write('Enter desired accuracy (e.g., 0.0001):'), nl,
        read(Accuracy),
        (squareroot(Number, Result, Accuracy) ->
            write('Square root: '), write(Result), nl,
            write('Verification: '), 
            Squared is Result * Result,
            write(Squared), nl
        ;
            write('Unable to calculate square root.')
        )
    ;
        write('Please enter a positive number.')
    ),
    write('Do you want to find another square root? (yes/no)'), nl,
    read(Continue),
    (Continue == yes -> square_root_finder ; true).

% Main menu
main_menu :-
    write('CS331 Assignment V'), nl,
    write('1. Duplicate Checker'), nl,
    write('2. Square Root Finder'), nl,
    write('3. Exit'), nl,
    write('Choose an option (1-3):'), nl,
    read(Choice),
    (Choice == 1 -> 
        duplicate_checker, 
        main_menu
    ; Choice == 2 -> 
        square_root_finder, 
        main_menu
    ; Choice == 3 ->
        write('Goodbye!'), nl
    ;
        write('Invalid option. Try again.'), nl,
        main_menu
    ).

% Start the program
:- initialization(main_menu).