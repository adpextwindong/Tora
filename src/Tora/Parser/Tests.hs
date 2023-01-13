{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tora.Parser.Tests where

import Data.Either
import Data.Either.Extra
import Data.ByteString.Lazy.Char8 (ByteString)
import Test.HUnit

import Tora.QQ
import qualified Tora.Lexer as L
import Tora.Parser
import Tora.AST (Program)

import Tora.Parser.Tests.ExprTests
import Tora.Parser.Tests.DeclarationTests
----------------
-- TEST SUITE --
----------------

runParserTests = runTestTT testParser

testParser :: Test
testParser = TestList
  [testDeclaration]

t1 :: ByteString
t1 = [tigerSrc| function foo() = nil function bar () = nil|]

t2 = displayAST . fromRight' $ runParser t1
