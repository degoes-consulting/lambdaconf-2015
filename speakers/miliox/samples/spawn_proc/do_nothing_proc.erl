-module(do_nothing_proc).
-export([main/0, init/0]).

init() ->
    erlang:load_nif("./do_nothing_nif", 0).

main() ->
    Parent = self(),
    % T0 = os:timestamp(),
    T0 = timestamp(),
    spawn(fun() -> Parent ! ok end),
    receive
        ok -> ok
    end,
    T1 = timestamp(),
    % T1 = os:timestamp(),

    % {T0_Mega, T0_Sec, T0_Micro} = T0,
    % {T1_Mega, T1_Sec, T1_Micro} = T1,

    % Sec2Micro = 1000 * 1000,
    % Mega2Micro = 1000 * 1000 * Sec2Micro,

    % TotalT0 = T0_Mega * Mega2Micro + T0_Sec * Sec2Micro + T0_Micro,
    % TotalT1 = T1_Mega * Mega2Micro + T1_Sec * Sec2Micro + T1_Micro,

    TotalT0 = T0,
    TotalT1 = T1,

    io:format("deltaTime: ~w~n", [diff_timestamp(TotalT1, TotalT0)]).

timestamp() -> 0.

diff_timestamp(_T1, _T0) -> ok.
