{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Tora.Parser
  ( parseTiger
  , testParse
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid (First (..))
import Test.HUnit
import Data.Either
import Data.Either.Extra

import qualified Tora.Lexer as L
import Tora.QQ
import Tora.AST
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
  ":="      { L.RangedToken (L.TVarDecEquals) _ }
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
  '.'               { L.RangedToken (L.TDot) _ }
  do                { L.RangedToken (L.TDo) _ }
  to                { L.RangedToken (L.TTo) _ }
  integerLiteral    { L.RangedToken (L.TIntegerLit _) _ }
  stringLiteral     { L.RangedToken (L.TStringLit _) _ }
  '('               { L.RangedToken (L.TParenLeft) _ }
  ')'               { L.RangedToken (L.TParenRight) _ }
  '{'               { L.RangedToken (L.TBraceLeft) _ }
  '}'               { L.RangedToken (L.TBraceRight) _ }
  '['               { L.RangedToken (L.TBracketLeft) _ }
  ']'               { L.RangedToken (L.TBracketRight) _ }
  ':'               { L.RangedToken (L.TColon) _ }
  ';'               { L.RangedToken (L.TSemicolon) _ }
  ','               { L.RangedToken (L.TComma) _ }
  '+'               { L.RangedToken (L.TPLUS) _ }
  '-'               { L.RangedToken (L.TMINUS) _ }
  '*'               { L.RangedToken (L.TMUL) _ }
  '/'               { L.RangedToken (L.TDIV) _ }
  '='               { L.RangedToken (L.TEQUAL) _ }
  "<>"              { L.RangedToken (L.TNEQUAL) _ }
  '>'               { L.RangedToken (L.TGT) _ }
  '<'               { L.RangedToken (L.TLT) _ }
  ">="              { L.RangedToken (L.TGTE) _ }
  "<="              { L.RangedToken (L.TLTE) _ }
  '&'               { L.RangedToken (L.TBAnd) _ }
  '|'               { L.RangedToken (L.TBor) _ }

%left ":="
--TODO associativity tests
%left '*'
%left '/'
%left '+'
%left '-'

%nonassoc '='
%nonassoc "<>"
%nonassoc '>'
%nonassoc '<'
%nonassoc ">="
%nonassoc "<="

%right '&'
%right '|'

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
               : var name optional(typeAnnotation) ":=" expr
                  { VarDeclaration (L.rtRange $1 <-> info $5) $2 $3 $5 }


funDeclaration :: { Declaration L.Range }
               : function name '(' optional(tyFields) ')' optional(typeAnnotation) '=' expr
                  { FunDeclaration (L.rtRange $1 <-> info $8) $2 (flattenMaybe $4) $6 $8 }

typeAnnotation :: { Type L.Range }
               : ':' ty { $2 }

exprs :: { Expr L.Range }
      : expr many(semicolonExpr) { case $2 of
                                     [] -> ExprSeq (info $1) ($1 : $2)
                                     _ -> ExprSeq (info $1 <-> listInfo $2) ($1 : $2)
                                     }

semicolonExpr :: { Expr L.Range }
          : ';' expr { $2 }

expr :: { Expr L.Range }
     : '(' exprs ')' { $2 }
     | nil { NilExpr (L.rtRange $1) }
     | integerLiteral { unTok $1 (\range (L.TIntegerLit v) -> IntLitExpr (L.rtRange $1) v) }
     | stringLiteral { unTok $1 (\range (L.TStringLit v) -> StringLitExpr (L.rtRange $1) v) }
     | let declarations in end { LetExpr (L.rtRange $1 <-> L.rtRange $4) $2 (NoValueExpr (L.rtRange $3 <-> L.rtRange $4)) }
     | let declarations in exprs end { LetExpr (L.rtRange $1 <-> info $4) $2 $4 }
     | if expr then expr %shift { IFThenExpr (L.rtRange $1 <-> info $4) $2 $4 }
     | if expr then expr else expr %shift { IFThenElseExpr (L.rtRange $1 <-> info $6) $2 $4 $6 }
     | while expr do expr %shift { WhileExpr (L.rtRange $1 <-> info $4) $2 $4 }
     | lvalue ":=" expr { AssignmentExpr (info $1 <-> info $3) $1 $3 }
     | for name ":=" expr to expr do expr %shift { ForExpr (L.rtRange $1 <-> info $8) $2 $4 $6 $8 }
     | break { BreakExpr (L.rtRange $1) }
     | name '(' ')' { FunCallExpr (info $1 <-> L.rtRange $3) $1 [] }
     | name '(' expr  many(commaExpr) ')' { FunCallExpr (info $1 <-> L.rtRange $5) $1 ($3 : $4) }
     --TODO introduce typeid newtype around name.

     | name '[' expr ']' of expr %shift { ArrayInitExpr (info $1 <-> info $6) $1 $3 $6 }
     | lvalue { LValueExpr (info $1) $1 }

     | typeid '{' '}' { RecordInitExpr (info $1 <-> L.rtRange $3) $1 [] }
     | typeid '{' typeFieldInit many(commaTypeFieldInit) '}' { RecordInitExpr (info $1 <-> L.rtRange $5) $1 ($3 : $4) }
     | '(' ')' { NoValueExpr (L.rtRange $1 <-> L.rtRange $2) }
     | '-' expr      { UnaryNegate (L.rtRange $1 <-> info $2) $2 }

     | expr '*' expr { BinOpExpr (info $1 <-> info $3) $1 (MulOp $ L.rtRange $2) $3 }
     | expr '/' expr { BinOpExpr (info $1 <-> info $3) $1 (DivOp $ L.rtRange $2) $3 }
     | expr '+' expr { BinOpExpr (info $1 <-> info $3) $1 (PlusOp $ L.rtRange $2) $3 }
     | expr '-' expr { BinOpExpr (info $1 <-> info $3) $1 (MinusOp $ L.rtRange $2) $3 }

     | expr '=' expr { BinOpExpr (info $1 <-> info $3) $1 (EqualOp $ L.rtRange $2) $3 }
     | expr "<>" expr { BinOpExpr (info $1 <-> info $3) $1 (NEqualOp $ L.rtRange $2) $3 }
     | expr '>' expr { BinOpExpr (info $1 <-> info $3) $1 (GTOp $ L.rtRange $2) $3 }
     | expr '<' expr { BinOpExpr (info $1 <-> info $3) $1 (LTOp $ L.rtRange $2) $3 }
     | expr ">=" expr { BinOpExpr (info $1 <-> info $3) $1 (GTEOp $ L.rtRange $2) $3 }
     | expr "<=" expr { BinOpExpr (info $1 <-> info $3) $1 (LTEOp $ L.rtRange $2) $3 }
     | expr '&' expr { BinOpExpr (info $1 <-> info $3) $1 (BoolAndOp $ L.rtRange $2) $3 }
     | expr '|' expr { BinOpExpr (info $1 <-> info $3) $1 (BoolOrOp $ L.rtRange $2) $3 }

lvalue :: { LValue L.Range }
lvalue : name         { LValueBase (info $1) $1 }
       | lvalueNotId  { $1 }

lvalueNotId :: { LValue L.Range }
       : lvalue '.' name { LValueDot (info $1 <-> info $3) $1 $3 }
       | name '[' expr ']' { LValueArray (info $1 <-> L.rtRange $4) (LValueBase (info $1) $1) $3 }
       | lvalueNotId '[' expr ']' { LValueArray (info $1 <-> L.rtRange $4) $1 $3 }


commaTypeFieldInit :: { (Name L.Range, Expr L.Range) }
                   : ',' name '=' expr { ($2, $4) }

typeFieldInit :: { (Name L.Range, Expr L.Range) }
              : name '=' expr { ($1,$3) }

commaExpr :: { Expr L.Range }
commaExpr : ',' expr { $2 }

ty :: { Type L.Range }
   : name { TVar (info $1) $1 }
   | '{' optional(tyFields) '}' { TRecord (L.rtRange $1 <-> L.rtRange $3) (concat $2) }
   | arrayOf ty                 { TArray (L.rtRange $1 <-> info $2) $2 }

typeid :: { Name L.Range }
       : name { $1 }

name :: { Name L.Range }
     : identifier { unTok $1 (\range (L.TIdentifier name) -> Name range name) }

tyFields :: { [TyField L.Range] }
       : tyField many(commaTyField)    { $1 : $2 }

tyField :: { TyField L.Range }
            : name ':' ty                  { TyField (info $1 <-> info $3) $1 $3 }

commaTyField :: { TyField L.Range }
            : ',' name ':' ty              { TyField (info $2 <-> info $4) $2 $4 }
{
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
        getLast [] = info x

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
  ,testWhileLoop
  ,testForLoop
  ,testBreakExpr
  ,testFunCallExpr
  ,testArrayInitExpr
  ,testRecordInitExpr
  ,testNoValueExprs
  ,testBooleanOperators
  ,testArithemtic
  ,testComparison
  ,testUnaryNegation
  ,testLValue]

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
  [ testFunDeclEmpty
  , testFunDeclNoAnnotation
  , testFunDeclWithAnnotation
  , testFunDeclWithBadAnnotation ]

testFunDeclEmpty = TestCase $ do
  let input = [tigerSrc| function foo() = 5 |]
  let output = testParse input

  let test = \case
        (ProgDecls _ [(FunDeclaration _ (Name _ "foo") [] Nothing
          (IntLitExpr _ 5))]) -> True
        _ -> False

  assertBool "funDecl without args" $ test output

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
  let input = [tigerSrc| ( 5; "foo" ) |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (ExprSeq _ [(IntLitExpr _ 5),(StringLitExpr _ "foo")])) -> True
        _ -> False

  assertBool "Paren Expr Test" $ test output

testLetExpr
  = TestList
  [testLetExprMulti
  ,testLetEmptyDecs]

testLetExprMulti = TestCase $ do
  let input = [tigerSrc| let var foo := 5 var bar := nil in 6; 7 end |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LetExpr _ [VarDeclaration _ (Name _ "foo") Nothing (IntLitExpr _ 5),VarDeclaration _ (Name _ "bar") Nothing (NilExpr _)] (ExprSeq _ [IntLitExpr _ 6,IntLitExpr _ 7]))) -> True
        _ -> False

  assertBool "LetExprMulti Test" $ test output

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

testForLoop = TestCase $ do
  let input = [tigerSrc| for foo := 5 to 6 do 1 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (ForExpr _ (Name _ "foo") (IntLitExpr _ 5) (IntLitExpr _ 6) (IntLitExpr _ 1))) -> True
        _ -> False

  assertBool "For loop test" $ test output

testBreakExpr = TestCase $ do
  let input = [tigerSrc| break |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BreakExpr _)) -> True
        _ -> False

  assertBool "Break expr test" $ test output

testFunCallExpr
  = TestList
  [testFunCallExprMultiArgs
  ,testFunCallExprNoArgs
  ,testFunCallExprSingleArg ]

testFunCallExprMultiArgs = TestCase $ do
  let input = [tigerSrc| foo(1,2) |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (FunCallExpr _ (Name _ "foo") [IntLitExpr _ 1, IntLitExpr _ 2])) -> True
        _ -> False

  assertBool "FunCall multi arg test" $ test output

testFunCallExprNoArgs = TestCase $ do
  let input = [tigerSrc| foo() |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (FunCallExpr _ (Name _ "foo") [])) -> True
        _ -> False

  assertBool "FunCall no arg test" $ test output

testFunCallExprSingleArg = TestCase $ do
  let input = [tigerSrc| foo(1) |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (FunCallExpr _ (Name _ "foo") [IntLitExpr _ 1])) -> True
        _ -> False

  assertBool "FunCall no arg test" $ test output

testArrayInitExpr = TestCase $ do
  let input = [tigerSrc| foo[2] of 1 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (ArrayInitExpr _ (Name _ "foo") (IntLitExpr _ 2) (IntLitExpr _ 1))) -> True
        _ -> False

  assertBool "Array init expr multi test" $ test output

testRecordInitExpr
  = TestList
  [testRecordInitEmpty
  ,testRecordInitSingleTyField
  ,testRecordInitMultiTyField]

testRecordInitEmpty = TestCase $ do
  let input = [tigerSrc| foo { } |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (RecordInitExpr _ (Name _ "foo") [])) -> True
        _ -> False

  assertBool "Record init empty test" $ test output

testRecordInitSingleTyField = TestCase $ do
  let input = [tigerSrc| foo {baz = 3} |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (RecordInitExpr _ (Name _ "foo") [(Name _ "baz", IntLitExpr _ 3)])) -> True
        _ -> False

  assertBool "Record init single field" $ test output

testRecordInitMultiTyField = TestCase $ do
  let input = [tigerSrc| foo {baz = 3, quux = 5} |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (RecordInitExpr _ (Name _ "foo") [(Name _ "baz", IntLitExpr _ 3)
                                                     ,(Name _ "quux", IntLitExpr _ 5)])) -> True
        _ -> False

  assertBool "Record init multi field" $ test output

testNoValueExprs
  = TestList
  [testNoValueUnit
  ,testNoValueLetExpr]

testNoValueUnit = TestCase $ do
  let input = [tigerSrc| () |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (NoValueExpr _)) -> True
        _ -> False

  assertBool "Unit No Value Test" $ test output

testNoValueLetExpr = TestCase $ do
  let input = [tigerSrc| let in end |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LetExpr _ [] (NoValueExpr _))) -> True
        _ -> False

  assertBool "NoValue LetExpr Test" $ test output

testBooleanOperators
  = TestList
  [testBoolAndOp
  ,testBoolOrOp]

--TODO more complicated unit tests for conditional expressions and bool ops
testBoolAndOp = TestCase $ do
  let input = [tigerSrc| 5 & 0 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (BoolAndOp _) (IntLitExpr _ 0))) -> True
        _ -> False

  assertBool "And Boolean Operator Test" $ test output

testBoolOrOp = TestCase $ do
  let input = [tigerSrc| 5 | 0 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (BoolOrOp _) (IntLitExpr _ 0))) -> True
        _ -> False

  assertBool "And Boolean Operator Test" $ test output

testArithemtic
  = TestList
  [testMulOp
  ,testDivOp
  ,testPlusOp
  ,testMinusOp]

testMulOp = TestCase $ do
  let input = [tigerSrc| 5 * 2 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (MulOp _) (IntLitExpr _ 2))) -> True
        _ -> False

  assertBool "Mul Op Test" $ test output

testDivOp = TestCase $ do
  let input = [tigerSrc| 5 / 2 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (DivOp _) (IntLitExpr _ 2))) -> True
        _ -> False

  assertBool "Mul Op Test" $ test output

testPlusOp = TestCase $ do
  let input = [tigerSrc| 5 + 2 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (PlusOp _) (IntLitExpr _ 2))) -> True
        _ -> False

  assertBool "Plus Op Test" $ test output

testMinusOp = TestCase $ do
  let input = [tigerSrc| 5 - 2 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (MinusOp _) (IntLitExpr _ 2))) -> True
        _ -> False

  assertBool "Plus Op Test" $ test output

testComparison
  = TestList
  [testEqualOp
  ,testNEqualOp
  ,testGTOp
  ,testLTOp
  ,testGTEOp
  ,testLTEOp]

testEqualOp = TestCase $ do
  let input = [tigerSrc| 5 = 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (EqualOp _) (IntLitExpr _ 4))) -> True
        _ -> False

  assertBool "Equal Op Test" $ test output

testNEqualOp = TestCase $ do
  let input = [tigerSrc| 5 <> 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (NEqualOp _) (IntLitExpr _ 4))) -> True
        _ -> False

  assertBool "NEqual Op Test" $ test output

testGTOp = TestCase $ do
  let input = [tigerSrc| 5 > 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (GTOp _) (IntLitExpr _ 4))) -> True
        _ -> False

  assertBool "Greater Than Op Test" $ test output

testLTOp = TestCase $ do
  let input = [tigerSrc| 5 < 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (LTOp _) (IntLitExpr _ 4))) -> True
        _ -> False

  assertBool "Less Than Op Test" $ test output

testGTEOp = TestCase $ do
  let input = [tigerSrc| 5 >= 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (GTEOp _) (IntLitExpr _ 4))) -> True
        _ -> False

  assertBool "Greater Than Equal Op Test" $ test output

testLTEOp = TestCase $ do
  let input = [tigerSrc| 5 <= 4 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (IntLitExpr _ 5) (LTEOp _) (IntLitExpr _ 4))) -> True
        _ -> False

  assertBool "Less Than Equal Op Test" $ test output

testUnaryNegation = TestCase $ do
  let input = [tigerSrc| - 5 + 2 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (BinOpExpr _ (UnaryNegate _ (IntLitExpr _ 5)) (PlusOp _) (IntLitExpr _ 2))) -> True
        _ -> False

  assertBool "Unary Negate Test" $ test output

testLValue
  = TestList
  [testLValueSingle
  ,testLValueDot
  ,testLValueArray
  ,testAssignment]

--TODO assignment

testLValueSingle = TestCase $ do
  let input = [tigerSrc| foo |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LValueExpr _ (LValueBase _ (Name _ "foo")))) -> True
        _ -> False

  assertBool "LValue Single Test" $ test output

testLValueDot = TestCase $ do
  let input = [tigerSrc| foo.bar |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LValueExpr _ (LValueDot _ (LValueBase _ (Name _ "foo")) (Name _ "bar")))) -> True
        _ -> False

  assertBool "LValue Dot Test" $ test output

testLValueArray = TestCase $ do
  let input = [tigerSrc| foo[3] |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (LValueExpr _ (LValueArray _ (LValueBase _ (Name _ "foo")) (IntLitExpr _ 3)))) -> True
        _ -> False

  assertBool "LValue Array Test" $ test output

testAssignment = TestCase $ do
  let input = [tigerSrc| foo := 5 |]
  let output = testParse input

  let test = \case
        (ProgExpr _ (AssignmentExpr _ (LValueBase _ (Name _ "foo")) (IntLitExpr _ 5))) -> True
        _ -> False

  assertBool "Assignment Test" $ test output

testParse :: ByteString -> Program L.Range
testParse = fromRight' . runParser

testShouldFail :: ByteString -> Bool
testShouldFail = isLeft . runParser

t1 :: ByteString
t1 = [tigerSrc| foo[3] |]
t2 = displayAST . fromRight' $ runParser t1

displayAST :: (Functor f) => f a -> f ()
displayAST = fmap (const ())

flattenMaybe :: Maybe [a] -> [a]
flattenMaybe Nothing = []
flattenMaybe (Just xs) = xs
}
