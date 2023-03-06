{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tora.Test.TypeCheckerTests where

import Tora.QQ
import Tora.Parser
import Tora.AST
import Tora.TypeChecker

import Test.HUnit

-- Invalid Programs
reservedTyNameTestInt = testParse [tigerSrc| type int = Nil |]
reservedTyNameTestString = testParse [tigerSrc| type string = Nil |]

tyDecAdjacentReservedFail = testParse [tigerSrc| type Foo = int
                                                 type string = Foo |]
-- Valid Programs
tyDecSimpleTest = testParse [tigerSrc| type foo = int |]

--TODO impl scope insertion
tyDecAdjacent = testParse [tigerSrc| type foo = int
                                     type bar = foo |]


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
