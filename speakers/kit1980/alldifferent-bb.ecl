:- lib(ic).
:- lib(branch_and_bound).
model(Vars) :-
    Vars :: 0..9,
    alldifferent(Vars),
    Cost #= sum(Vars),
    minimize(
        search(Vars, 0, input_order, indomain, complete, []),
        Cost
    ).
