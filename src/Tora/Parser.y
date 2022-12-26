{
{-# LANGUAGE DeriveFoldable #-}

module Tora.Parser
  ( parseTiger
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Tora.Lexer as L
}

%name parseTiger
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
  equal             { L.RangedToken (L.TEQUAL) _ }
  nequal            { L.RangedToken (L.TNEQUAL) _ }
  greaterThan       { L.RangedToken (L.TGT) _ }
  lessThan          { L.RangedToken (L.TLT) _ }
  greaterThanEqual  { L.RangedToken (L.TGTE) _ }
  lessThanEqual     { L.RangedToken (L.TLTE) _ }
  booleanAnd        { L.RangedToken (L.TBAnd) _ }
  booleanOr         { L.RangedToken (L.TBor) _ }
  endOfFile         { L.RangedToken (L.TEOF) _ }

%%

empty : {}

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}
