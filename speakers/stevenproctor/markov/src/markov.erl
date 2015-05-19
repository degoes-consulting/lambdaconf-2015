%%%-------------------------------------------------------------------
%%% @author proctor
%%% @copyright (C) 2015, proctor
%%% @doc
%%%
%%% @end
%%% Created : 2015-02-12 12:12:24.849303
%%%-------------------------------------------------------------------
-module(markov).

-export([start/0,
         prime/1,
         generate_chain/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok | {error, term()}.
start() ->
    application:start(markov).

-spec prime(string()) -> ok.
prime(Text) ->
    markov_generator:parse_text(Text).

-spec generate_chain(string(), non_neg_integer()) -> string().
generate_chain(Word, WordCount) ->
    markov_generator:generate_chain(Word, WordCount).


%%%===================================================================
%%% Internal functions
%%%===================================================================




