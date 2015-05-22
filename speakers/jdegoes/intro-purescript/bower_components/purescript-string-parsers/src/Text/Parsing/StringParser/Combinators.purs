module Text.Parsing.StringParser.Combinators where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser

lookAhead :: forall a. Parser a -> Parser a
lookAhead p = Parser \ps fc sc -> unParser p ps fc (\s _ -> sc s ps)

many :: forall a. Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: forall a. Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a : as)

(<?>) :: forall a. Parser a -> String -> Parser a
(<?>) p msg = p <|> fail msg

fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser (\s fc sc -> unParser (f (fix f)) s fc sc)

between :: forall a open close. Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  a <- p
  close
  return a

option :: forall a. a -> Parser a -> Parser a
option a p = p <|> return a

optional :: forall a. Parser a -> Parser Unit
optional p = (p >>= \_ -> return unit) <|> return unit

optionMaybe :: forall a. Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

sepBy :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  return (a : as)

sepEndBy :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

sepEndBy1 :: forall a sep. Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      return (a : as)) <|> return [a]

endBy1 :: forall a sep. Parser a -> Parser sep -> Parser [a]
endBy1 p sep = many1 $ do
  a <- p
  sep
  return a

endBy :: forall a sep. Parser a -> Parser sep -> Parser [a]
endBy p sep = many $ do
  a <- p
  sep
  return a

chainr :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p f a = chainr1 p f <|> return a

chainl :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p f a = chainl1 p f <|> return a

chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p f = do
  a <- p
  chainl1' p f a

chainl1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> return a

chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall a. Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     return $ f' a a') <|> return a

choice :: forall a. [Parser a] -> Parser a
choice []   = fail "Nothing to parse"
choice [x]  = x
choice (x:xs) = x <|> choice xs

manyTill :: forall a end. Parser a -> Parser end -> Parser [a]
manyTill p end = scan
  where
  scan = (end *> return []) <|> do x <- p
                                   xs <- scan
                                   return (x:xs)
