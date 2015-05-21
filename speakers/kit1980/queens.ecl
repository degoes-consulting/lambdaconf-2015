:- lib(ic).
:- lib(listut). % for nth1

queens(N, Qs) :-
    length(Qs, N),
    Qs :: 1..N,
    alldifferent(Qs),
    ( for(I, 1, N), param(Qs, N) do
        ( for(J, I + 1, N), param(Qs, I) do
            nth1(I, Qs, X),
            nth1(J, Qs, Y),
            abs(X - Y) #\= J - I
        )
    ),
    search(Qs, 0, input_order, indomain, complete, []).
