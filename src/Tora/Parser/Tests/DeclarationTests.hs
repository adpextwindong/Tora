{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tora.Parser.Tests.DeclarationTests (
    testDeclaration
  ) where

import Test.HUnit
import Tora.QQ
import Tora.Parser.Tests.Util (stdParserTest)
import Tora.AST

testDeclaration
  = TestList
  [testAdjacentTypeDecl
  ,testAdjacentFunDecls]

testAdjacentTypeDecl
  = stdParserTest "Test Adjacent Type Decls"
  [tigerSrc| type foo = id type bar = id |]
  (\case
    (ProgDecls _ [TypeDeclaration _ [(Name _ "foo", TVar _ (Name _ "id"))
                                     ,(Name _ "bar", TVar _ (Name _ "id"))]]) -> True
    _ -> False)

testAdjacentFunDecls
  = stdParserTest "Test Adjacent Fun Decls"
  [tigerSrc| function foo() = nil function bar() = nil |]
  (\case
    (ProgDecls _ [FunDeclaration _
      [FunDecl _ (Name _ "foo") [] Nothing (NilExpr _)
      ,FunDecl _ (Name _ "bar") [] Nothing (NilExpr _)]]) -> True
    _ -> False)
