{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

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
  '('               { L.RangedToken (L.TParenLeft) _ }
  ')'               { L.RangedToken (L.TParenRight) _ }
  '{'               { L.RangedToken (L.TBraceLeft) _ }
  '}'               { L.RangedToken (L.TBraceRight) _ }
  '['               { L.RangedToken (L.TBracketLeft) _ }
  ']'               { L.RangedToken (L.TBracketRight) _ }
  ':'               { L.RangedToken (L.TColon) _ }
  semicolon         { L.RangedToken (L.TSemicolon) _ }
  ','               { L.RangedToken (L.TComma) _ }
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

-- UTILS
optional(p)
  : { Nothing }
  | p { Just $1 }

many_rev(p)
  :         { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

-- Grammar

declaration :: { Declaration L.Range }
            : typeDeclaration             { $1 }
            -- TODO VarDecl
            -- TODO FunDecl

typeDeclaration :: { Declaration L.Range }
                : type name '=' ty { TypeDeclaration (L.rtRange $1 <-> info $4) $2 $4 }

ty :: { Type L.Range }
   : name { TVar (info $1) $1 }
   | '{' optional(record) '}' { TRecord (L.rtRange $1 <-> L.rtRange $3) (concat $2) }

name :: { Name L.Range }
     : identifier { unTok $1 (\range (L.TIdentifier name) -> Name range name) }

record :: { [RecordField L.Range] }
       : recordField many(commaRecordField)    { $1 : $2 }

recordField :: { RecordField L.Range }
            : name ':' ty                  { RecordField (info $1 <-> info $3) $1 $3 }

commaRecordField :: { RecordField L.Range }
            : ',' name ':' ty              { RecordField (info $2 <-> info $4) $2 $4 }
{

---------
-- AST --
---------

data Declaration a
  = TypeDeclaration a (Name a) (Type a)
  -- | VarDeclaration
  -- | FunDeclaration
  deriving (Functor, Foldable, Show)

data Name a
  = Name a ByteString
  deriving (Functor, Foldable, Show)

data Type a
  = TVar a (Name a)
  | TRecord a [RecordField a]
  deriving (Functor, Foldable, Show)

data RecordField a
  = RecordField a (Name a) (Type a)
  deriving (Functor, Foldable, Show)


-----------
-- UTILS --
-----------

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

-----------
-- TESTS --
-----------

runParserTests = runTestTT testParser

testParser :: Test
testParser = TestList
  [testTypeId
  ,testRecordType]

testTypeId :: Test
testTypeId = TestCase $ do
  let input = [tigerSrc| type foo = int |]
  let output = fromRight' $ runParser input
  let test = \case
        (TypeDeclaration _ (Name _ "foo") (TVar _ (Name _ "int"))) -> True
        _ -> False
  assertBool "type id test" $ test output

testRecordType = TestList
  [testRecordTypeMulti
  ,testRecordTypeLeadingComma
  ,testRecordTypeSingleField
  ,testRecordTypeNoFields]

testRecordTypeMulti :: Test
testRecordTypeMulti = TestCase $ do
  let input = [tigerSrc|type any = {any : int, foo : baz} |]
  let output = fromRight' $ runParser input

  let test = \case
        (TypeDeclaration _ (Name _ "any")
          (TRecord _ [(RecordField _ (Name _ "any") (TVar _ (Name _ "int")))
                     ,(RecordField _ (Name _ "foo") (TVar _ (Name _ "baz")))
                      ])) -> True
        _ -> False

  assertBool "type record test" $ test output

testRecordTypeLeadingComma :: Test
testRecordTypeLeadingComma = TestCase $ do
  let input = [tigerSrc|type any = {,any : int, foo : baz} |]
  assertBool "type record leading comma fails" $ isLeft $ runParser input

testRecordTypeSingleField = TestCase $ do
  let input = [tigerSrc|type any = {any : int} |]
  let output = fromRight' $ runParser input

  let test = \case
        (TypeDeclaration _ (Name _ "any")
          (TRecord _ [(RecordField _ (Name _ "any") (TVar _ (Name _ "int")))])) -> True
        _ -> False

  assertBool "type record single field" $ test output

testRecordTypeNoFields = TestCase $ do
  let input = [tigerSrc|type any = {} |]
  let output = fromRight' $ runParser input

  let test = \case
        (TypeDeclaration _ (Name _ "any")
          (TRecord _ [])) -> True
        _ -> False

  assertBool "type record no fields" $ test output

t1 :: ByteString
t1 = [tigerSrc| type any = {any : int, foo : baz} |]
t2 = displayAST . fromRight' $ runParser t1

displayAST :: (Functor f) => f a -> f ()
displayAST = fmap (const ())

}
