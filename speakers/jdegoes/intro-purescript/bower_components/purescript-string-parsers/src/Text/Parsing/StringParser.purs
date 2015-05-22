module Text.Parsing.StringParser where

import Data.Either (Either(..))

import Control.Alt
import Control.Alternative
import Control.MonadPlus
import Control.Plus

type Pos = Number

--
-- Strings are represented as a string with an index from the
-- start of the string.
--
-- { str: s, pos: n } is interpreted as the substring of s
-- starting at index n.
--
-- This allows us to avoid repeatedly finding substrings
-- every time we match a character.
--
type PosString = { str :: String, pos :: Pos }

--
-- The type of parsing errors
--
data ParseError = ParseError String

instance showParseError :: Show ParseError where
  show (ParseError msg) = msg

--
-- A parser is represented as a function which takes a pair of
-- continuations for failure and success.
--
data Parser a = Parser (forall r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r)

unParser :: forall a r. Parser a -> PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r
unParser (Parser p) = p

runParser :: forall a. Parser a -> String -> Either ParseError a
runParser p s = unParser p { str: s, pos: 0 } (\_ err -> Left err) (\a _ -> Right a)

--
-- Parser type class instances
--

instance functorParser :: Functor Parser where
  (<$>) f p = Parser (\s fc sc ->
    unParser p s fc (\a s' -> sc (f a) s'))

instance applyParser :: Apply Parser where
  (<*>) f x = Parser (\s fc sc ->
    unParser f s fc (\f' s' ->
      unParser x s' fc (\x' s'' -> sc (f' x') s'')))

instance applicativeParser :: Applicative Parser where
  pure a = Parser (\s _ sc -> sc a s)

instance altParser :: Alt Parser where
  (<|>) p1 p2 = Parser (\s fc sc ->
    unParser p1 s (\pos msg ->
      if s.pos == pos
      then unParser p2 s fc sc
      else fc pos msg)
      sc)

instance plusParser :: Plus Parser where
  empty = fail "No alternative"

instance alternativeParser :: Alternative Parser

instance bindParser :: Bind Parser where
  (>>=) p f = Parser (\s fc sc ->
    unParser p s fc (\a s' ->
      unParser (f a) s' fc sc))

instance monadParser :: Monad Parser

instance monadPlusParser :: MonadPlus Parser

fail :: forall a. String -> Parser a
fail msg = Parser (\{ pos = pos } fc _ -> fc pos (ParseError msg))

try :: forall a. Parser a -> Parser a
try p = Parser (\(s@{ pos = pos }) fc sc -> unParser p s (\_ -> fc pos) sc)
