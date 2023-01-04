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

%name parseTiger program
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
  ';'         { L.RangedToken (L.TSemicolon) _ }
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

program :: { Program L.Range }
        : expr { ProgExpr (info $1) $1 }
        | declarations { ProgDecls (listInfo $1) $1 }

declarations :: { [Declaration L.Range] }
             : many(declaration) { $1 }

declaration :: { Declaration L.Range }
            : typeDeclaration             { $1 }
            | varDeclaration              { $1 }
            | funDeclaration              { $1 }

typeDeclaration :: { Declaration L.Range }
                : type name '=' ty { TypeDeclaration (L.rtRange $1 <-> info $4) $2 $4 }

varDeclaration :: { Declaration L.Range }
               : var name optional(typeAnnotation) varDecEquals expr
                  { VarDeclaration (L.rtRange $1 <-> info $5) $2 $3 $5 }


funDeclaration :: { Declaration L.Range }
               : function name '(' tyFields ')' optional(typeAnnotation) '=' expr
                  { FunDeclaration (L.rtRange $1 <-> info $8) $2 $4 $6 $8 }

typeAnnotation :: { Type L.Range }
               : ':' ty { $2 }

exprs :: { Expr L.Range }
      : expr many(semicolonExpr) { ExprSeq (info $1 <-> listInfo $2) ($1 : $2) }

semicolonExpr :: { Expr L.Range }
          : ';' expr { $2 }

expr :: { Expr L.Range }
     : '(' exprs ')' { $2 }
     | nil { NilExpr (L.rtRange $1) }
     | integerLiteral { unTok $1 (\range (L.TIntegerLit v) -> IntLitExpr (L.rtRange $1) v) }
     | stringLiteral { unTok $1 (\range (L.TStringLit v) -> StringLitExpr (L.rtRange $1) v) }
     | let declarations in exprs end { LetExpr (L.rtRange $1 <-> info $4) $2 $4 }
     | if expr then expr %shift { IFThenExpr (L.rtRange $1 <-> info $4) $2 $4 }
     | if expr then expr else expr { IFThenElseExpr (L.rtRange $1 <-> info $6) $2 $4 $6 }
     | while expr do expr { WhileExpr (L.rtRange $1 <-> info $4) $2 $4 }

elseExpr :: { Expr L.Range }
         : else expr { $2 }

ty :: { Type L.Range }
   : name { TVar (info $1) $1 }
   | '{' optional(tyFields) '}' { TRecord (L.rtRange $1 <-> L.rtRange $3) (concat $2) }
   | arrayOf ty                 { TArray (L.rtRange $1 <-> info $2) $2 }

name :: { Name L.Range }
     : identifier { unTok $1 (\range (L.TIdentifier name) -> Name range name) }

tyFields :: { [TyField L.Range] }
       : tyField many(commaTyField)    { $1 : $2 }

tyField :: { TyField L.Range }
            : name ':' ty                  { TyField (info $1 <-> info $3) $1 $3 }

commaTyField :: { TyField L.Range }
            : ',' name ':' ty              { TyField (info $2 <-> info $4) $2 $4 }
{

---------
-- AST --
---------

data Declaration a
  = TypeDeclaration a (Name a) (Type a)
  | VarDeclaration a (Name a) (Maybe (Type a)) (Expr a)
  | FunDeclaration a (Name a) [TyField a] (Maybe (Type a)) (Expr a)
  deriving (Functor, Foldable, Show)

data Name a
  = Name a ByteString
  deriving (Functor, Foldable, Show)

data Type a
  = TVar a (Name a)
  | TRecord a [TyField a]
  | TArray a (Type a)
  deriving (Functor, Foldable, Show)

data TyField a
  = TyField a (Name a) (Type a)
  deriving (Functor, Foldable, Show)

data Expr a
  = NilExpr a
  | IntLitExpr a Int
  | StringLitExpr a ByteString
  | ExprSeq a [Expr a]
  | LetExpr a [Declaration a] (Expr a) -- | Uses an ExprSeq
  | IFThenExpr a (Expr a) (Expr a)
  | IFThenElseExpr a (Expr a) (Expr a) (Expr a)
  | WhileExpr a (Expr a) (Expr a)
  deriving (Functor, Foldable, Show)

data Program a
  = ProgExpr a (Expr a)
  | ProgDecls a [Declaration a]
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

listInfo :: Foldable f => [f L.Range] -> L.Range
listInfo (x:xs) = info x <-> getLast xs
  where getLast (x:[]) = info x
        getLast (x:xs) = getLast xs

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

----------------
-- TEST SUITE --
----------------

runParserTests = runTestTT testParser

testParser :: Test
testParser = TestList
  [testTypeId
  ,testRecordType
  ,testVarDecl
  ,testFunDecl
  ,testArrayDecl
  ,testExprProgram
  ,testExprSeqAndParens
  ,testLetExpr
  ,testIfThenElseExpr
  ,testStringLiteral
  ,testWhileLoop ]

testTypeId :: Test
testTypeId = TestCase $ do
  let input = [tigerSrc| type foo = int |]
  let output = fromRight' $ runParser input
  let test = \case
        (ProgDecls _ [(TypeDeclaration _ (Name _ "foo") (TVar _ (Name _ "int")))]) -> True
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
        (ProgDecls _ [(TypeDeclaration _ (Name _ "any")
          (TRecord _ [(TyField _ (Name _ "any") (TVar _ (Name _ "int")))
                     ,(TyField _ (Name _ "foo") (TVar _ (Name _ "baz")))
                      ]))]) -> True
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
        (ProgDecls _ [(TypeDeclaration _ (Name _ "any")
          (TRecord _ [(TyField _ (Name _ "any") (TVar _ (Name _ "int")))]))]) -> True
        _ -> False

  assertBool "type record single field" $ test output

testRecordTypeNoFields = TestCase $ do
  let input = [tigerSrc|type any = {} |]
  let output = fromRight' $ runParser input

  let test = \case
        (ProgDecls _ [(TypeDeclaration _ (Name _ "any")
          (TRecord _ []))]) -> True
        _ -> False

  assertBool "type record no fields" $ test output

testVarDecl
  = TestList
  [testVarDeclWithAnnotation
  ,testVarDeclNoAnnotation]

testVarDeclWithAnnotation = TestCase $ do
  let input = [tigerSrc| var foo : any := nil|]
  let output = fromRight' . runParser $ input

  let test = \case
        (ProgDecls _ [(VarDeclaration _ (Name _ "foo") (Just (TVar _ (Name _ "any")))
          (NilExpr _))]) -> True
        _ -> False

  assertBool "varDecl with type annotation" $ test output

testVarDeclNoAnnotation = TestCase $ do
  let input = [tigerSrc| var foo := 5 |]
  let output = testParse input

  let test = \case
        (ProgDecls _ [(VarDeclaration _ (Name _ "foo") Nothing (IntLitExpr _ 5))]) -> True
        _ -> False

  assertBool "varDecl without type annotation" $ test output

testFunDecl
  = TestList
  [ testFunDeclNoAnnotation
  , testFunDeclWithAnnotation
  , testFunDeclWithBadAnnotation ]

testFunDeclNoAnnotation = TestCase $ do
  let input = [tigerSrc| function foo(bar : int) = 5 |]
  let output = testParse input

  let test = \case
        (ProgDecls _ [(FunDeclaration _ (Name _ "foo") [TyField _ (Name _ "bar") (TVar _ (Name _ "int"))] Nothing
          (IntLitExpr _ 5))]) -> True
        _ -> False

  assertBool "funDecl without type annotation" $ test output

testFunDeclWithAnnotation = TestCase $ do
  let input = [tigerSrc| function foo(quux : int, baz : int) : int = 5 |]
  let output = testParse input

  let test = \case
        (ProgDecls _ [(FunDeclaration _ (Name _ "foo") [TyField _ (Name _ "quux") (TVar _ (Name _ "int"))
                                                       ,TyField _ (Name _ "baz") (TVar _ (Name _ "int"))]
                                        (Just (TVar _ (Name _ "int")))
                                        (IntLitExpr _ 5))]) -> True
        _ -> False

  assertBool "funDecl with type annotation" $ test output

testFunDeclWithBadAnnotation = TestCase $ do
  let input = [tigerSrc| function foo(bar, baz : int) : int = 5 |]
  assertBool "funDecl missing typefield type annotation fails" $ isLeft . runParser $ input

testArrayDecl = TestCase $ do
  let input = [tigerSrc| type intArray = array of int |]
  let output = testParse input

  let test = \case
        (ProgDecls _ [(TypeDeclaration _ (Name _ "intArray") (TArray _ (TVar _ (Name _ "int"))))]) -> True
        _ -> False

  assertBool "array type declaration" $ test output

testExprProgram = TestCase $ do
  let input = [tigerSrc| 5 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (IntLitExpr _ 5)) -> True
        _ -> False

  assertBool "ProgExpr test" $ test output

testExprSeqAndParens = TestCase $ do
  let input = [tigerSrc| ( 5 ) |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (ExprSeq _ [(IntLitExpr _ 5)])) -> True
        _ -> False

  assertBool "Paren Expr Test" $ test output

testLetExpr
  = TestList
  [testLetExprMulti
  ,testLetExprEmptyShouldFail
  ,testLetEmptyDecs]

testLetExprMulti = TestCase $ do
  let input = [tigerSrc| let var foo := 5 var bar := nil in 6; 7 end |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LetExpr _ [VarDeclaration _ (Name _ "foo") Nothing (IntLitExpr _ 5),VarDeclaration _ (Name _ "bar") Nothing (NilExpr _)] (ExprSeq _ [IntLitExpr _ 6,IntLitExpr _ 7]))) -> True

  assertBool "LetExprMulti Test" $ test output

testLetExprEmptyShouldFail = TestCase $ do
  let input = [tigerSrc| let in end |]
  assertBool "LetExprMulti Test" $ testShouldFail input

testLetEmptyDecs = TestCase $ do
  let input = [tigerSrc| let in 5 end |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LetExpr _ [] (ExprSeq _ [IntLitExpr _ 5]))) -> True
        _ -> False

  assertBool "LetExprMulti Test" $ test output

testIfThenElseExpr
  = TestList
  [testIfThenElse
  ,testMissingIfThenElse
  ,testIfThenElseDangling]

testIfThenElse = TestCase $ do
  let input = [tigerSrc| if 1 then 10 else 20 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (IFThenElseExpr _ (IntLitExpr _ 1) (IntLitExpr _ 10) (IntLitExpr _ 20))) -> True
        _ -> False

  assertBool "Standard ife test" $ test output

testIfThenElseDangling = TestCase $ do
  let input = [tigerSrc| if 1 then if 2 then 3 else 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (IFThenExpr _ (IntLitExpr _ 1) (IFThenElseExpr _ (IntLitExpr _ 2) (IntLitExpr _ 3) (IntLitExpr _ 4)))) -> True

        _ -> False

  assertBool "Dangling ife test" $ test output

testMissingIfThenElse = TestCase $ do
  let input = [tigerSrc| if then 10 else 20 |]
  assertBool "Missing if clause should fail." $ testShouldFail input

testStringLiteral = TestCase $ do
  let input = [tigerSrc| "foo" |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (StringLitExpr _ "foo")) -> True
        _ -> False

  assertBool "String literal test" $ test output

testWhileLoop = TestCase $ do
  let input = [tigerSrc| while 1 do 2 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (WhileExpr _ (IntLitExpr _ 1) (IntLitExpr _ 2))) -> True
        _ -> False

  assertBool "While loop test" $ test output


testParse :: ByteString -> Program L.Range
testParse = fromRight' . runParser

testShouldFail :: ByteString -> Bool
testShouldFail = isLeft . runParser

t1 :: ByteString
t1 = [tigerSrc| while 1 do 2 |]
t2 = displayAST . fromRight' $ runParser t1

displayAST :: (Functor f) => f a -> f ()
displayAST = fmap (const ())

}
