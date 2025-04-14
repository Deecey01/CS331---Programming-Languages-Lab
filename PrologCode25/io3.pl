getSymptoms :-
    write('enter symptoms'),nl,
    read(Symptom),
    New_Symptom = [Symptom],
    append([],[New_symptom],[[]|New_symptom]),
    write('are their more symptoms? y or n '),
    read('Answer'),
    Answer =:= y  -> getSymptoms.
