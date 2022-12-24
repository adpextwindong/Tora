{
{-# LANGUAGE FlexibleContexts #-}

module Tora.Lexer where

import Control.Monad.Except
import Data.Char (toLower)
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanumeric = [_a-zA-Z0-9]
$eol = [\n]

tokens :-
  -- Whitespace insensitive
  $eol        ;
  $white+     ;

  -- Identifier TODO
  -- Comments TODO
  -- Symbols TODO
  -- Operators TODO


{

--Lexer Types


data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState

alexInitUserState = AlexUserState


alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _ ,_) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  = TOp TOperator String AlexPosn
  | EOF
  deriving (Eq, Show)

data TOperator = EQUAL
  deriving (Eq, Show)

}
