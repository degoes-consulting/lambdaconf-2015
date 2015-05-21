:- lib(ic).
model(Vars) :-
    Vars :: 0..9,
    alldifferent(Vars),
    search(Vars, 0, input_order, indomain, complete, []).
