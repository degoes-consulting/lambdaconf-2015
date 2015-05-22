module Text.Parsing.StringParser.Expr where

import Data.Either
import Data.Foldable

import Control.Alt

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators

data Assoc = AssocNone | AssocLeft | AssocRight

data Operator a = Infix   (Parser (a -> a -> a)) Assoc
                | Prefix  (Parser (a -> a))
                | Postfix (Parser (a -> a))

type OperatorTable a = [[Operator a]]

type SplitAccum a = { rassoc :: [Parser (a -> a -> a)]
                    , lassoc :: [Parser (a -> a -> a)]
                    , nassoc :: [Parser (a -> a -> a)]
                    , prefix :: [Parser (a -> a)]
                    , postfix :: [Parser (a -> a)] }

buildExprParser :: forall a. OperatorTable a -> Parser a -> Parser a
buildExprParser operators simpleExpr =
  let
    makeParser term ops =
      let
        accum     = foldr splitOp { rassoc: [], lassoc: [], nassoc: [], prefix: [], postfix: [] } ops

        rassocOp  = choice accum.rassoc
        lassocOp  = choice accum.lassoc
        nassocOp  = choice accum.nassoc
        prefixOp  = choice accum.prefix <?> ""
        postfixOp = choice accum.postfix <?> ""

        postfixP = postfixOp <|> return id
        prefixP = prefixOp <|> return id
      in do
        x <- termP prefixP term postfixP
        rassocP x rassocOp prefixP term postfixP
          <|> lassocP x lassocOp prefixP term postfixP
          <|> nassocP x nassocOp prefixP term postfixP
          <|> return x
          <?> "operator"

    splitOp :: forall a. Operator a -> SplitAccum a -> SplitAccum a
    splitOp (Infix op AssocNone)  accum = accum { nassoc  = op: accum.nassoc }
    splitOp (Infix op AssocLeft)  accum = accum { lassoc  = op: accum.lassoc }
    splitOp (Infix op AssocRight) accum = accum { rassoc  = op: accum.rassoc }
    splitOp (Prefix  op)          accum = accum { prefix  = op: accum.prefix }
    splitOp (Postfix op)          accum = accum { postfix = op: accum.postfix }

    rassocP :: forall a b c. a -> Parser (a -> a -> a) -> Parser (b -> c) -> Parser b -> Parser (c -> a) -> Parser a
    rassocP x rassocOp prefixP term postfixP = do
      f <- rassocOp
      y <- do
        z <- termP prefixP term postfixP
        rassocP1 z rassocOp prefixP term postfixP
      return (f x y)

    rassocP1 :: forall a b c. a -> Parser (a -> a -> a) -> Parser (b -> c) -> Parser b -> Parser (c -> a) -> Parser a
    rassocP1 x rassocOp prefixP term postfixP = rassocP x rassocOp prefixP term postfixP <|> return x

    lassocP :: forall a b c. a -> Parser (a -> a -> a) -> Parser (b -> c) -> Parser b -> Parser (c -> a) -> Parser a
    lassocP x lassocOp prefixP term postfixP = do
      f <- lassocOp
      y <- termP prefixP term postfixP
      lassocP1 (f x y) lassocOp prefixP term postfixP

    lassocP1 :: forall a b c. a -> Parser (a -> a -> a) -> Parser (b -> c) -> Parser b -> Parser (c -> a) -> Parser a
    lassocP1 x lassocOp prefixP term postfixP = lassocP x lassocOp prefixP term postfixP <|> return x

    nassocP :: forall a b c d e. a -> Parser (a -> d -> e) -> Parser (b -> c) -> Parser b -> Parser (c -> d) -> Parser e
    nassocP x nassocOp prefixP term postfixP = do
      f <- nassocOp
      y <- termP prefixP term postfixP
      return (f x y)

    termP :: forall a b c. Parser (a -> b) -> Parser a -> Parser (b -> c) -> Parser c
    termP prefixP term postfixP = do
      pre   <- prefixP
      x     <- term
      post  <- postfixP
      return (post (pre x))

  in foldl (makeParser) simpleExpr operators
