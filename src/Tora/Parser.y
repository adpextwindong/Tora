{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Tora.Parser
  ( parseTiger
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Test.HUnit
import Data.Either
import Data.Either.Extra

import qualified Tora.Lexer as L
import Tora.QQ
}

%name parseTiger declaration
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.TEOF _ }

%token

  identifier        { L.RangedToken (L.TIdentifier _) _ }
  type              { L.RangedToken (L.TType) _ }
  arrayOf           { L.RangedToken (L.TArrayOf) _ }
  of                { L.RangedToken (L.TOf) _ }
  varDecEquals      { L.RangedToken (L.TVarDecEquals) _ }
  var               { L.RangedToken (L.TVar) _ }
  function          { L.RangedToken (L.TFun) _ }
  if                { L.RangedToken (L.TIf) _ }
  then              { L.RangedToken (L.TThen) _ }
  else              { L.RangedToken (L.TElse) _ }
  let               { L.RangedToken (L.TLet) _ }
  in                { L.RangedToken (L.TIn) _ }
  end               { L.RangedToken (L.TEnd) _ }
  nil               { L.RangedToken (L.TNil) _ }
  while             { L.RangedToken (L.TWhile) _ }
  for               { L.RangedToken (L.TFor) _ }
  break             { L.RangedToken (L.TBreak) _ }
  dot               { L.RangedToken (L.TDot) _ }
  do                { L.RangedToken (L.TDo) _ }
  integerLiteral    { L.RangedToken (L.TIntegerLit _) _ }
  floatLiteral      { L.RangedToken (L.TFloatLit _) _ }
  stringLiteral     { L.RangedToken (L.TStringLit _) _ }
  parenLeft         { L.RangedToken (L.TParenLeft) _ }
  parenRight        { L.RangedToken (L.TParenRight) _ }
  braceLeft         { L.RangedToken (L.TBraceLeft) _ }
  braceRight        { L.RangedToken (L.TBraceRight) _ }
  bracketLeft       { L.RangedToken (L.TBracketLeft) _ }
  bracketRight      { L.RangedToken (L.TBracketRight) _ }
  colon             { L.RangedToken (L.TColon) _ }
  semicolon         { L.RangedToken (L.TSemicolon) _ }
  comma             { L.RangedToken (L.TComma) _ }
  plus              { L.RangedToken (L.TPLUS) _ }
  minus             { L.RangedToken (L.TMINUS) _ }
  mul               { L.RangedToken (L.TMUL) _ }
  div               { L.RangedToken (L.TDIV) _ }
  '='               { L.RangedToken (L.TEQUAL) _ }
  nequal            { L.RangedToken (L.TNEQUAL) _ }
  greaterThan       { L.RangedToken (L.TGT) _ }
  lessThan          { L.RangedToken (L.TLT) _ }
  greaterThanEqual  { L.RangedToken (L.TGTE) _ }
  lessThanEqual     { L.RangedToken (L.TLTE) _ }
  booleanAnd        { L.RangedToken (L.TBAnd) _ }
  booleanOr         { L.RangedToken (L.TBor) _ }
  endOfFile         { L.RangedToken (L.TEOF) _ }

%%

declaration :: { Declaration L.Range }
            : typeDeclaration             { $1 }
            -- TODO VarDecl
            -- TODO FunDecl

typeDeclaration :: { Declaration L.Range }
                : type name '=' ty { TypeDeclaration (L.rtRange $1 <-> info $4) $2 $4 }

ty :: { Type L.Range }
   : name { TVar (info $1) $1 }

name :: { Name L.Range }
     : identifier { unTok $1 (\range (L.TIdentifier name) -> Name range name) }
{

data Declaration a
  = TypeDeclaration a (Name a) (Type a)
  -- | VarDeclaration
  -- | FunDeclaration
  deriving (Foldable, Show)

data Name a
  = Name a ByteString
  deriving (Foldable, Show)

data Type a
  = TVar a (Name a)
  deriving (Foldable, Show)

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

runParser src = L.runAlex src parseTiger

runParserTests = runTestTT testParser

testParser :: Test
testParser = TestList
  [testTypeId]

testTypeId :: Test
testTypeId = TestCase $ do
  let input = [tigerSrc|type foo = int |]
  let output = fromRight' $ runParser input
  let test = \case
        (TypeDeclaration _ (Name _ "foo") (TVar _ (Name _ "int"))) -> True
        _ -> False
  assertBool "type id test" $ test output

}
