:- lib(ic).
main :-
    X #= 2 + 3,
    X =:= 5,
    writeln("Looks good!").
