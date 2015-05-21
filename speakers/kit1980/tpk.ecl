f(T, Y) :-
   Y is sqrt(abs(T)) + 5*T^3.
main :-
    read(As), length(As, N),
    reverse(As, Rs),
    ( foreach(Ai, Rs), for(I, N - 1, 0, -1) do
        Bi is f(Ai),
        ( Bi > 400 ->
            printf("%w TOO LARGE\n", I)
        ;
            printf("%w %w\n", [I, Bi])
        )
    ).