-- | This is the entry point to the game, and contains the main game logic.
module Game where  
  import Control.Monad.Eff(Eff())
  import Data.Either(Either(..))

  import Game.State
  import Game.Input
  import Game.Driver
  
  myGame :: Game State Input
  myGame = { initial: initial, describe: describe, parse: parse, update: update }
    where
      initial :: State
      initial = {}

      describe :: State -> String
      describe s = "You are standing no where, with nothing around"

      update :: State -> Input -> Either String State
      update s Look = Right s
      update s (Take v) = Left ("There is no " ++ v ++ " to take!")
      update s (Drop v) = Left ("You are not carrying a " ++ v ++ "!")

  main = runGame myGame

-- | This is the game state. This is very bare bones at present. During the 
-- | workshop, you will add additional state into here to make the game 
-- | more interesting.
module Game.State where
  type State = {}

-- | This is the module responsible for parsing user input, and converting input
-- | into data types that are easier to work with than raw strings.
-- |
-- | You can add more commands to the game here.
module Game.Input
  ( Input(..)
  , parse
  ) where  

  import Data.Either(Either(..))

  import Control.Alt((<|>))
  import Control.Apply((<*), (*>))

  import Text.Parsing.StringParser
  import Text.Parsing.StringParser.Combinators
  import Text.Parsing.StringParser.String
  import Text.Parsing.StringParser.Expr

  data Input = Look | Take String | Drop String

  trailingWs :: Parser Unit
  trailingWs = void <<< many1 $ string " "

  command :: forall a. String -> a -> Parser a
  command t a = string t *> (trailingWs <|> eof) *> return a

  word :: Parser String
  word = go "" <* (optional trailingWs) where
    go v = eof *> return v <|> do
      char <- anyChar
      if char == " " then return v else go (v ++ char)

  input :: Parser Input
  input = (look <|> take <|> drop) <* eof
    where
      look = command "look" Look

      take = command "take" Take <*> word

      drop = command "drop" Drop <*> word

  parse :: String -> Either String Input
  parse s = case runParser input s of
        Left (ParseError e) -> Left $ e
        Right i             -> Right i

-- | This is a low-level module that defines what a game is and how to run it.
-- | It can handle any type of game, as long as it fits the mold specified herein.
-- |
-- | You should not need to modify this file during the workshop.
module Game.Driver
  ( Game()
  , GAME()
  , runGame 
  ) where

  import Control.Monad.Eff(Eff())
  import Data.Function(Fn2(), runFn2)
  import Data.Either(Either(..), either)

  foreign import data GAME :: !
  
  type Game s i = {
    initial  :: s,
    describe :: s -> String,
    parse    :: String -> Either String i,
    update   :: s -> i -> Either String s }

  runGame :: forall s i. Game s i -> Eff (game :: GAME) Unit
  runGame g = runFn2 _runGame either g

  foreign import _runGame """
    function _runGame(either, game) {
      return function() {
        var state = game.initial;

        var button  = document.querySelector("#button");
        var input   = document.querySelector("#input");
        var content = document.querySelector("#content");

        content.innerHTML = game.describe(state);

        var onInput = function(e) {
          var cmd = input.value;
          var parsed = game.parse(cmd.toLowerCase());

          either(function(error) {
            content.innerHTML += "<br><br>" + error;
          })(function(input) {
            either(function(error) {
              content.innerHTML += "<br><br>" + error;
            })(function(state2) {
              state = state2;

              content.innerHTML += "<br><br>" + cmd + "<br><br>" + game.describe(state);
            })(game.update(state)(input));
          })(parsed);

          input.value = '';
        };

        button.onclick = onInput;

        input.onkeypress = function(e) {
          if (e.keyCode == 13) {
            e.preventDefault();
            onInput(e);
          }
        };
      };
    }
  """ :: forall s i. Fn2 (forall a b c. (a -> c) -> (b -> c) -> (Either a b) -> c) (Game s i) (Eff (game :: GAME) Unit)
