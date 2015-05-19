%%%-------------------------------------------------------------------
%%% @author proctor
%%% @copyright (C) 2015, proctor
%%% @doc
%%%
%%% @end
%%% Created : 2015-02-06 12:15:05.506471
%%%-------------------------------------------------------------------
-module(markov_word).

-behaviour(gen_server).

%% API
-export([start_link/1,
         add_following_word/2,
         pick_next_word_after/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {following_words = [] :: [string()]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()}
                            | ignore
                            | {error, {already_started, pid()}}
                            | {error, term()}.
start_link(WordKey) when is_atom(WordKey) ->
    gen_server:start_link({local, WordKey}, ?MODULE, [], []).


-spec add_following_word(string(), string()) -> ok.
add_following_word(Word, FollowingWord) ->
    WordPid = find_process_for_word(Word),
    gen_server:call(WordPid, {add_following_word, FollowingWord}).

-spec pick_next_word_after(string()) -> string().
pick_next_word_after(Word) ->
    WordPid = find_process_for_word(Word),
    gen_server:call(WordPid, {pick_next_word}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call({add_following_word, string()}, _From, #state{}) -> {reply, ok, #state{}}
                ;({pick_next_word}, _From, #state{}) -> {reply, string(), #state{}}.
handle_call(_Request, _From, State) ->
        {reply, undefined, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec find_process_for_word(string()) -> pid().
find_process_for_word(Word) ->
    undefined.

-spec register_word(atom()) -> pid().
register_word(Word) ->
    undefined.

-spec add_word_to_list([string()], string()) -> [string()].
add_word_to_list(_Words, _Word) ->
    undefined.

-spec pick_next_word([string(),...]) -> string().
pick_next_word(_Words) ->
    undefined.

-spec pick_random([string(), ...]) -> string().
pick_random(_List) ->
    undefined.

-spec get_registered_name_for_word(string()) -> atom().
get_registered_name_for_word(Word) ->
    list_to_atom("markov_word::" ++ Word).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_word_to_list_test() ->
    ?assertMatch(["a", "b", "c"], add_word_to_list(["b", "c"], "a")),
    ?assertMatch(["a"], add_word_to_list([], "a")).

-endif.
