{-# LANGUAGE InstanceSigs #-}

module Instances (ParseResult (Result, Error), Parser (..), readInt, ParseError(..), isErrorResult) where

import           Control.Applicative (Alternative (..))

-- $setup
-- >>> let p = \n -> Parser (\x -> Result x n)
-- >>> let add = \n -> \m -> Parser (\x -> Result x (n+m))

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  | NonPositiveInt
  deriving (Eq, Show)

data ParseResult a
  = Error ParseError
  | Result Input a
  deriving (Eq)

type Input = String

newtype Parser a = Parser {parse :: Input -> ParseResult a}


-- Result Instances

instance Show a => Show (ParseResult a) where
  show :: ParseResult a -> String
  show (Result i a) = "Result >" ++ i ++ "< " ++ show a
  show (Error UnexpectedEof) = "Unexpected end of stream"
  show (Error NonPositiveInt) = "Negative number in the stream"
  show (Error (UnexpectedChar c)) = "Unexpected character: " ++ show [c]
  show (Error (UnexpectedString s)) = "Unexpected string: " ++ show s
  show (Error (ExpectedEof i)) = "Expected end of stream, but got >" ++ show i ++ "<"

instance Functor ParseResult where
  fmap :: (a -> b) -> ParseResult a -> ParseResult b
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error e)    = Error e

-- Parser Instances

-- | Functor instance for a parser
--
-- >>> parse ((+1) <$> Parser (`Result` 1)) "hello"
-- Result >hello< 2

--  >>> parse ((+1) <$> Parser (const (Error UnexpectedEof))) "hello"
-- Unexpected end of stream
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (fmap f . p)

-- | Implement this before Applicative Parser!
--
-- >>> parse (p 1 >>= add 2) ""
-- Result >< 3
--
-- prop> \i j -> parse (p i >>= add j) "" == Result "" (i + j)
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser p) f = Parser $ \i -> case p i of
    Result rest x -> parse (f x) rest
    Error e       -> Error e

-- |
--
-- >>> parse (Parser (`Result` (+1)) <*> Parser (`Result` 1)) "hello"
-- Result >hello< 2
--
-- >>> parse (pure (+1) <*> Parser (`Result` 1)) "hello"
-- Result >hello< 2
--
-- >>> parse (pure (+1) <*> (pure 1 :: Parser Int)) "hello"
-- Result >hello< 2
instance Applicative Parser where
  -- creates a Parser that always succeeds with the given input
  pure :: a -> Parser a
  pure = Parser . flip Result

  -- or
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = p >>= (<$> q)

-- |
--
-- 'Alternative' is used for parsers that support choice (alternative) and
-- failure (empty). In the context of this 'Parser' type:
--
-- 1. 'empty' represents a parser that always fails
--
-- 2. '<|>' combines two parsers. It tries the first parser on the input,
-- and if it fails it tries the second parser.
--
-- see https://tgdwyer.github.io/haskell3/#alternative
--
-- >>> parse (pure 1 <|> empty) "world, hello!"
-- Result >world, hello!< 1
-- >>> parse (empty <|> pure 1) "world"
-- Result >world< 1
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (Error UnexpectedEof)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) pa pb = Parser $ \x -> case parse pa x of
    Error _ -> parse pb x
    r       -> r

-- Support functions

isErrorResult :: ParseResult a -> Bool
isErrorResult (Error _) = True
isErrorResult _         = False

readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing
