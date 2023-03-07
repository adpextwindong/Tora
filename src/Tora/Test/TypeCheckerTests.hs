{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tora.Test.TypeCheckerTests where

import Tora.QQ
import Tora.Parser
import qualified Tora.Lexer as L
import Tora.AST
import Tora.TypeChecker

import Test.HUnit

-- Invalid Programs
reservedTyNameTestInt = testParse [tigerSrc| type int = Nil |]
reservedTyNameTestString = testParse [tigerSrc| type string = Nil |]

tyDecAdjacentReservedFail = testParse [tigerSrc| type Foo = int
                                                 type string = Foo |]

tyDecAdjacentMissing = testParse [tigerSrc| type foo = int
                                            type bar = QUUXX |]

varDeclRawToNil = testParse [tigerSrc| var a := nil |]

-- Valid Programs
tyDecSimpleTest = testParse [tigerSrc| type foo = int |]

tyDecAdjacentValid = testParse [tigerSrc| type foo = int
                                          type bar = foo |]

tyIntVarRawSimple = testParse [tigerSrc| var a := 5 |]
tyStringVarRawSimple = testParse [tigerSrc| var a := "foo" |]
--TODO! tyIntVarTypedSimple = testParse [tigerSrc| var a : int := 5 |]
--TODO! [tigerSrc| var a = "foo"
--                 var b : int := a |]
--
-- Valid ProgExprs

tyNilProgExpr = testParse [tigerSrc| nil |]
tyIntLitProgExpr = testParse [tigerSrc| 5 |]
tyStringLitProgExpr = testParse [tigerSrc| "foo" |]

{-
tyDecLookupChaining = testParse [tigerSrc| type foo = int
                                           function bar() =
                                             let type quux = foo in nil
                                           end |]
-}

validTests = TestList [
    validTyCheck "Simple single type decl" tyDecSimpleTest
   ,validTyCheck "Adjacent Valid type decls" tyDecAdjacentValid

   ,validTyCheck "Simple int lit var decl" tyIntVarRawSimple
   ,validTyCheck "Simple string lit var decl" tyStringVarRawSimple

   ,validTyCheck "Simple nil expr" tyNilProgExpr
   ,validTyCheck "Simple int lit expr" tyIntLitProgExpr
   ,validTyCheck "Simple string lit expr" tyStringLitProgExpr
  ]

invalidTests = TestList [
    TestLabel "Reserved type keyword tests" (TestList [
      invalidTyCheck "Int Reserved Base Type" ReservedBaseTyNameError reservedTyNameTestInt
     ,invalidTyCheck "String Reserved Base Type" ReservedBaseTyNameError reservedTyNameTestString
    ])
    ,invalidTyCheck "Adjacent Reserved Base Type" ReservedBaseTyNameError tyDecAdjacentReservedFail

    ,TestLabel "Nil Handling Failure Tests" (TestList [
      invalidTyCheck "Untyped Var Decl Assigned as Nil" RawVarNilDeclError varDeclRawToNil
    ])

  ]

tests = TestList [
    TestLabel "Valid Programs" validTests
   ,TestLabel "Invalid Programs" invalidTests
   ]

runTyCheckTests = runTestTT tests

validTyCheck :: String -> Program L.Range -> Test
validTyCheck caseName inputProg = TestCase $ assertEqual caseName (Right ()) (typeCheckProg inputProg)

invalidTyCheck :: String -> TypeError -> Program L.Range -> Test
invalidTyCheck caseName err inputProg = TestCase $ assertEqual caseName (Left err) (typeCheckProg inputProg)


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
--   let t : string = ... in t
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
