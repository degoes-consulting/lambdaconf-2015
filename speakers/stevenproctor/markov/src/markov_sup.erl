%%%-------------------------------------------------------------------
%% @doc markov top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(markov_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
        RestartStrategy = one_for_all,
        MaxRestarts = 1000,
        MaxSecondsBetweenRestarts = 3600,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Children = [markov_word_supervisor(),
                    markov_generator_supervisor()],

        {ok, {SupFlags, Children}}.
%%====================================================================
%% Internal functions
%%====================================================================

markov_word_supervisor() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    {markov_word_sup, {markov_word_sup, start_link, []},
        Restart, Shutdown, Type, [markov_word_sup]}.

markov_generator_supervisor() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    {markov_generator_sup, {markov_generator_sup, start_link, []},
        Restart, Shutdown, Type, [markov_generator_sup]}.
