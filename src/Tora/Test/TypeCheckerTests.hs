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

untypedVarDeclAssignedAsNilTest = invalidTyCheck "Untyped Var Decl Assigned as Nil" RawVarNilDeclError varDeclRawToNil


varDeclTests = TestList [
  untypedVarDeclAssignedAsNilTest
  ,validTyCheck "Int Ty Var Decl" tyIntVarTypedSimple
  ,validTyCheck "String Ty Var Decl" tyStringVarTypedSimple
  ,invalidTyCheck "Var Expr Type Decl Mismatch" TypeAliasMismatchError $ testParse
    [tigerSrc| var x : string := 5|]

   ,invalidTyCheck "Type Alias Mismatch Base Int" AssertTyError $ testParse
    [tigerSrc| type foo = int
               var x : foo := "five"|]

 --TODO
  ]

tyNilProgExpr = testParse [tigerSrc| nil |]
tyIntLitProgExpr = testParse [tigerSrc| 5 |]
tyStringLitProgExpr = testParse [tigerSrc| "foo" |]

exprTests = TestList [
   validTyCheck "Simple nil expr" tyNilProgExpr
   ,validTyCheck "Simple int lit expr" tyIntLitProgExpr
   ,validTyCheck "Simple string lit expr" tyStringLitProgExpr
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
validTyCheck caseName inputProg = TestLabel caseName $ TestCase $ assertEqual caseName (Right ()) (typeCheckProg inputProg)

invalidTyCheck :: String -> TypeError -> Program L.Range -> Test
invalidTyCheck caseName err inputProg = TestLabel caseName $ TestCase $ assertEqual caseName (Left err) (typeCheckProg inputProg)


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
