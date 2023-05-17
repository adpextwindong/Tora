{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tora.Test.TypeCheckerTests where

import Tora.QQ
import Tora.Parser
import qualified Tora.Lexer as L
import Tora.AST
import Tora.TypeChecker

import Test.HUnit
import Tora.Parser (testParse)
import Tora.TypeChecker (TypeError(TypeAliasMismatchError))

reservedTyNameTestInt = testParse [tigerSrc| type int = Nil |]
reservedTyNameTestString = testParse [tigerSrc| type string = Nil |]

tyDecAdjacentReservedFail = testParse [tigerSrc| type Foo = int
                                                 type string = Foo |]

tyDecAdjacentMissing = testParse [tigerSrc| type foo = int
                                            type bar = QUUXX |]


tyDecSimpleTest = testParse [tigerSrc| type foo = int |]

tyDecAdjacentValid = testParse [tigerSrc| type foo = int
                                          type bar = foo |]

tyIntVarRawSimple = testParse [tigerSrc| var a := 5 |]
tyStringVarRawSimple = testParse [tigerSrc| var a := "foo" |]

tyIntVarTypedSimple = testParse [tigerSrc| var a : int := 5 |]
tyStringVarTypedSimple = testParse [tigerSrc| var a : string := "five" |]
--TODO! [tigerSrc| var a = "foo"
--                 var b : int := a |]
--
-- Valid ProgExprs
{-
tyDecLookupChaining = testParse [tigerSrc| type foo = int
                                           function bar() =
                                             let type quux = foo in nil
                                           end |]
-}
-- RECORD
tyDecRecordSimpleTest = testParse [tigerSrc| type rec = { val : int } |]

typeDeclTests = TestList [
    validTyCheck "Simple single type decl" tyDecSimpleTest
   ,validTyCheck "Adjacent Valid type decls" tyDecAdjacentValid

   ,validTyCheck "Simple int lit var decl" tyIntVarRawSimple
   ,validTyCheck "Simple string lit var decl" tyStringVarRawSimple

   ,TestLabel "Reserved type keyword tests" (TestList [
      invalidTyCheck "Int Reserved Base Type" ReservedBaseTyNameError reservedTyNameTestInt
     ,invalidTyCheck "String Reserved Base Type" ReservedBaseTyNameError reservedTyNameTestString
    ])

    ,invalidTyCheck "Adjacent Reserved Base Type" ReservedBaseTyNameError tyDecAdjacentReservedFail


    ,validTyCheck "Simple Record Type Decl" tyDecRecordSimpleTest
  ]

varDeclRawToNil = testParse [tigerSrc| var a := nil |]

varDeclSimpleRecord = testParse [tigerSrc| type rec = { val : int }
                                           var foo := rec { val = 42 } |]

varDeclSimpleTypedRecord = testParse [tigerSrc|
                                          type rec = { val : int }
                                          var foo : rec := rec { val = 42 } |]

varDeclSplitTypedRecord = testParse [tigerSrc| type rec = { val : int }
                                          var foo : rec := rec { val = 42 } |]

varDeclSplitTypedRecordLong = testParse [tigerSrc| type rec = { val : int, quux : int }
                                          var foo : rec := rec { val = 42, quux = 666 } |]


varDeclSplitTypedRecordLongBroken = testParse [tigerSrc| type rec = { val : string, quux : string }
                                          var foo : rec := rec { val = 42, quux = 666 } |]


untypedVarDeclAssignedAsNilTest = invalidTyCheck "Untyped Var Decl Assigned as Nil" RawVarNilDeclError varDeclRawToNil

    --TODO {- type rec = { val : int } var foo : rec = rec { val = int } }
varDeclNoTypeDeclRec = testParse [tigerSrc| var foo : rec := rec { val = int } |]

varDeclMismatchRecord = testParse [tigerSrc| type a = { val : int }
                                             type b = { val : int }
                                             var foo : a := b { val = 1 } |] --Fails because of distinct record types

varDeclRecTypeAliasMismatch = testParse [tigerSrc| type foo = int
                                                   var bar : foo := baz { val = int } |]

undeclaredRecTypeVarDecl = testParse [tigerSrc| type rec = { val : int }
                                                 var foo : rec := baz { val = 1 } |]

varDeclTests = TestList [
  untypedVarDeclAssignedAsNilTest
  ,validTyCheck "Int Ty Var Decl" tyIntVarTypedSimple
  ,validTyCheck "String Ty Var Decl" tyStringVarTypedSimple
  ,invalidTyCheck "Var Expr Type Decl Mismatch" TypeAliasMismatchError $ testParse
    [tigerSrc| var x : string := 5|]

   ,invalidTyCheck "Type Alias Mismatch Base Int" AssertTyError $ testParse
    [tigerSrc| type foo = int
               var x : foo := "five"|]

  ,validTyCheck "Simple Var Record Var Decl" varDeclSimpleRecord
  ,validTyCheck "Typed Var Record Var Decl" varDeclSimpleTypedRecord
  ,validTyCheck "Split Typed Var Record Var Decl" varDeclSplitTypedRecord
  ,validTyCheck "Split Typed Var Record Var Decl Long" varDeclSplitTypedRecordLong
  ,invalidTyCheck "Split Typed Var Record Var Decl Long Broken" RecordExprTyFieldMismatch varDeclSplitTypedRecordLongBroken
  ,invalidTyCheck "Rec Type Var Decl with no type decl" AnonymousTypeUsageError varDeclNoTypeDeclRec
  ,invalidTyCheck "Mismatch Rec Type Var Decl" VarDeclTypeMismatchError varDeclMismatchRecord
  ,invalidTyCheck "Rec Against not rec type mismatch" TypeAliasMismatchError varDeclRecTypeAliasMismatch
  ,invalidTyCheck "Undeclared Rec Type Var Decl" VarDeclTypeMismatchError undeclaredRecTypeVarDecl
  ]

tyNilProgExpr = testParse [tigerSrc| nil |]
tyIntLitProgExpr = testParse [tigerSrc| 5 |]
tyStringLitProgExpr = testParse [tigerSrc| "foo" |]

exprSeqEx = testParse [tigerSrc| (5; "foo") |]

simpleLetExpr = testParse [tigerSrc| let var x := 5 in x end |]
simpleLetProg = testParse [tigerSrc| var x := 5
                                     var y := (let var x := "foo" in x end) |]

invalidLValueBaseInLetExpr = testParse [tigerSrc| var y := (let var x := "foo" in z end) |]

exprTests = TestList [
   validTyCheck "Simple nil expr" tyNilProgExpr
   ,validTyCheck "Simple int lit expr" tyIntLitProgExpr
   ,validTyCheck "Simple string lit expr" tyStringLitProgExpr
   ,validTyCheck "Simple Expr Seq" exprSeqEx
   ,validTyCheck "Simple LetExpr" simpleLetExpr
   ,validTyCheck "simple Let Prog" simpleLetProg
   ,invalidTyCheck "Invalid LValueBase In Let Expr" InvalidLValueBaseNameError invalidLValueBaseInLetExpr
   --TODO
  ]

astTests = TestList [
  TestLabel "Type Declaration Tests" typeDeclTests
  ,TestLabel "Var Decl Tests" varDeclTests
  ,TestLabel "Expr Tests" exprTests
  --TODO
  ]

nilHandlingTests = TestLabel "Nil Handling Failure Tests" $ TestList [
  untypedVarDeclAssignedAsNilTest
  --TODO
    ]


tests = TestList [
   astTests
   ,nilHandlingTests
   ]

runTyCheckTests = runTestTT tests

validTyCheck :: String -> Program L.Range -> Test
validTyCheck caseName inputProg = TestLabel caseName $ TestCase $ do
  result <- typeCheckProg inputProg
  assertEqual caseName (Right ()) result

invalidTyCheck :: String -> TypeError -> Program L.Range -> Test
invalidTyCheck caseName err inputProg = TestLabel caseName $ TestCase $ do
  result <- typeCheckProg inputProg
  assertEqual caseName (Left err) result


--
--
--
-- scopePopping = testParse [tigerSrc| let var v := 6 in nil end |]
-- funParse = testParse [tigerSrc| function foo(t : int) = 5 |]
-- funLetParse = testParse [tigerSrc| let function foo(t : int) = 5 in nil end |]

-- Properties
--
-- type a = ... a is visible to subsequent decs and scopes
--
-- Lexical scope shadowing
-- function foo(t : int) : string =
--   let t : string = ... in t = testParse [tigerSrc| type rec = { val : int } |]ssert
--
-- Lexical scope popping
-- let
  -- function foo(t : int) = Nil
  -- function bar() = t
  --  in Nil
--
-- Should fail to type check
--
{-
--TODO expand on these examples from pg 516 to do proper typechecking on. Give missing types, var decls etc...
testParse [tigerSrc| var a : my_record := nil |] -- OK
testParse [tigerSrc| a := nil |] -- OK
testParse [tigerSrc| if a <> nil then ... |] -- OK
testParse [tigerSrc| if nil <> a then ... |] -- OK
testParse [tigerSrc| if a = nil then ... |] -- OK
testParse [tigerSrc| function f(p: my_record) = ... f(nil) |] -- OK
testParse [tigerSrc| var a := nil |] -- ILLEGAL IMPLEMENTED
testParse [tigerSrc| if nil = nil then ... |] -- ILLEGAL
-}
