:- lib(ic).
%:- lib(gfd).

% SEND + MORE = MONEY
money(Ds) :-
    Ds = [S, E, N, D, M, O, R, Y],
    Ds :: [0..9],
    alldifferent(Ds),
    S #\= 0, M #\= 0,

                   1000 * S + 100 * E + 10 * N + D
                 + 1000 * M + 100 * O + 10 * R + E
    #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,

    labeling(Ds).