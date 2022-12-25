{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tora.Lexer where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Test.HUnit
import Data.Either
import Data.Either.Extra
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanumeric = [_a-zA-Z0-9]
$eol = [\n]
$quote = \"

tokens :-
  -- Whitespace insensitive
  <0> $white+     ;

  -- Identifiers

  --Comment and nested comment handling
  <0> "*/" { \p _ -> alexError $ "Error: unexpected closing comment" }
  <0> "/*" { nestComment `andBegin` comment }
  <comment> "/*" { nestComment }
  <comment> "*/" { unnestComment }
  <comment> \n ;
  <comment> . ;

  -- String Literal Handling
  -- We're just doing basic string literals. I don't care about the escape stuff for now.
  <0> $quote      { startStringLiteral `andBegin` stringLiteral }
  <stringLiteral> $quote { endStringLiteral }
  <stringLiteral> ($alphanumeric | $white)+ { tokString }

  <0> "type"      { tok TType }
  <0> "array of"  { tok TArrayOf }
  <0> "of"        { tok TOf }       -- See QUEENS.TIG
  <0> "function"  { tok TFun }

  <0> $digit+ { tokInteger }
  <0> $digit+\.$digit+ { tokFloat }

  <0> "="   { tok TEQUAL }
  <0> "+"   { tok TPLUS }
  <0> "-"   { tok TMINUS }
  <0> "*"   { tok TMUL }
  <0> "/"   { tok TDIV }
  <0> "<>"  { tok TNEQUAL }
  <0> ">"   { tok TGT }
  <0> "<"   { tok TLT }
  <0> ">="  { tok TGTE }
  <0> "<="  { tok TLTE }
  <0> "&"   { tok TBAnd }
  <0> "|"   { tok TBor }

  <0> "{" { tok TBraceLeft }
  <0> "}" { tok TBraceRight }
  <0> "(" { tok TParenLeft }
  <0> ")" { tok TParenRight }
  <0> "[" { tok TBracketLeft }
  <0> "]" { tok TBracketRight }
  <0> ":" { tok TColon }
  <0> ";" { tok TSemicolon }
  <0> "," { tok TComma }

  <0> "var"       { tok TVar }
  <0> ":="        { tok TVarDecEquals }
  <0> "if"        { tok TIf }
  <0> "then"      { tok TThen }
  <0> "else"      { tok TElse }
  <0> "."         { tok TDot }
  <0> "let"       { tok TLet }
  <0> "in"        { tok TIn }
  <0> "end"       { tok TEnd }
  <0> "nil"       { tok TNil }
  <0> "while"     { tok TWhile }
  <0> "do"        { tok TDo }
  <0> "for"       { tok TFor }
  <0> "break"     { tok TBreak }

  <0> $alpha $alphanumeric*   { tokId }
  --TODO String Literal
  --TODO Escape sequences

{


data Token
  = TIdentifier ByteString
  | TType                   -- For Type Decl
  | TArrayOf
  | TOf                     -- See QUEENS.TIG
  | TVarDecEquals           -- ":="
  | TVar
  | TFun
  | TIf
  | TThen
  | TElse
  | TLet
  | TIn
  | TEnd
  | TNil
  | TWhile
  | TFor
  | TBreak
  | TDot
  | TDo

  -- Literals
  | TIntegerLit Int
  | TFloatLit Float
  | TStringLit BS.ByteString
  --TODO String Literal

  -- Symbols
  | TParenLeft
  | TParenRight

  | TBraceLeft
  | TBraceRight

  | TBracketLeft
  | TBracketRight

  | TColon
  | TSemicolon
  | TComma

  -- Operator Symbols
  | TPLUS
  | TMINUS
  | TMUL
  | TDIV
  | TEQUAL
  | TNEQUAL
  | TGT
  | TLT
  | TGTE
  | TLTE
  | TBAnd
  | TBor

  | TEOF
    deriving (Eq, Show)

data TypeName = TInt | TStr
  deriving (Eq, Show)

data TOperator = EQUAL | PLUS | MINUS | MUL | DIV
  deriving (Eq, Show)

--type AlexAction result = AlexInput -> Int64 -> Alex result
--type AlexInput = (AlexPosn, Char, ByteString, Int64)

data AlexUserState = AlexUserState
  { nestLevel :: Int
  , stringLit :: Bool
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  , stringLit = False
  }

--TODO mtl?
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

startStringLiteral :: AlexAction RangedToken
startStringLiteral input len = do
  modify $ \s -> s{stringLit = True}
  skip input len

endStringLiteral :: AlexAction RangedToken
endStringLiteral input len = do
  state <- get
  put state{stringLit = False}
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
    { rtToken = TIntegerLit $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokFloat :: AlexAction RangedToken
tokFloat inp@(_,_, str, _) len =
  pure RangedToken
    { rtToken = TFloatLit $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokString :: AlexAction RangedToken
tokString inp@(_,_, str, _) len =
  pure RangedToken
    { rtToken = TStringLit $ BS.take len str
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

-- TESTS GENERATED BY ChatGPT


runLexerTests = runTestTT testLexer

testLexer :: Test
testLexer = TestList
  [ testIdentifier
    ,testComment
    ,testStringLiteral
    ,testOperators
    ,testBracesAndParens
    ,testKeywords
  ]

testIdentifier :: Test
testIdentifier = TestCase $ do
  let expected = [TIdentifier "foo", TIdentifier "bar123", TEOF]
  let input = "foo bar123"
  let output = fmap rtToken . fromRight' $ scanMany input
  assertEqual "Lexing identifiers" expected output

testComment :: Test
testComment = TestCase $ do
  let expected = [TEOF]
  let input = "/* this /* */is a comment */"
  let output = fmap rtToken . fromRight' $ scanMany input
  assertEqual "Lexing comments" expected output

--NOTE THIS DOES NOT SUPPORT NESTED ESCAPED STRINGS
testStringLiteral :: Test
testStringLiteral = TestCase $ do
  let expected = [TStringLit "hello world", TEOF]
  let input = "\"hello world\""
  let output = fmap rtToken . fromRight' $ scanMany input
  assertEqual "Lexing string literals" expected output

testOperators :: Test
testOperators = TestCase $ do
  let expected = [TPLUS, TMINUS, TMUL, TDIV, TNEQUAL, TGT, TLT, TGTE, TLTE, TBAnd, TBor, TEOF]
  let input = "+ - * / <> > < >= <= & |"
  let output = fmap rtToken . fromRight' $ scanMany input
  assertEqual "Lexing operators" expected output

testBracesAndParens :: Test
testBracesAndParens = TestCase $ do
  let expected = [TBraceLeft, TBraceRight, TParenLeft, TParenRight, TBracketLeft, TBracketRight, TEOF]
  let input = "{ } ( ) [ ]"
  let output = fmap rtToken . fromRight' $ scanMany input
  assertEqual "Lexing braces and parentheses" expected output

testKeywords :: Test
testKeywords = TestCase $ do
  let expected = [TType, TArrayOf, TOf, TFun, TVar, TVarDecEquals, TIf, TThen, TElse, TLet, TIn, TEnd, TNil, TWhile, TDo, TFor, TBreak, TEOF]
  let input = "type array of of function var := if then else let in end nil while do for break"
  let output = fmap rtToken . fromRight' $ scanMany input
  assertEqual "Lexing keywords" expected output

}
