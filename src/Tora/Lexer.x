{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tora.Lexer where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanumeric = [_a-zA-Z0-9]
$eol = [\n]

tokens :-
  -- Whitespace insensitive
  <0> $white+     ;

  --Comment and nested comment handling
  <0> "*/" { \p _ -> alexError $ "Error: unexpected closing comment" }
  <0> "/*" { nestComment `andBegin` comment }
  <comment> "/*" { nestComment }
  <comment> "*/" { unnestComment }
  <comment> \n ;
  <comment> . ;

  <0> $alpha $alphanumeric*   { tokId }
  <0> $digit+ { tokInteger }
  <0> $digit+\.$digit+ { tokFloat }
  <0> "=" { tok (TOp EQUAL) }

{

--type AlexAction result = AlexInput -> Int64 -> Alex result
--type AlexInput = (AlexPosn, Char, ByteString, Int64)

data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

nestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction RangedToken
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _ ,_) <- alexGetInput
  pure $ RangedToken TEOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  = TOp TOperator
  | TIdentifier ByteString
  | TEOF
  | TInteger Int
  | TFloat Float

  deriving (Eq, Show)

data TOperator = EQUAL
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_,_, str, _) len =
  pure RangedToken
    { rtToken = TIdentifier $ BS.take len str
    , rtRange = mkRange inp len
    }

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_,_, str, _) len =
  pure RangedToken
    { rtToken = TInteger $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokFloat :: AlexAction RangedToken
tokFloat inp@(_,_, str, _) len =
  pure RangedToken
    { rtToken = TFloat $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == TEOF
        then pure [output]
        else (output :) <$> go

}
