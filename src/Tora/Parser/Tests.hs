{-# LANGUAGE QuasiQuotes #-}
module Tora.Parser.Tests (
    testExpr
  ) where

import Tora.QQ
import Tora.Parser.Tests.ExprTests
import Tora.Parser
import Test.HUnit
import Data.Either.Extra
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Tora.Lexer as L
import Test.HUnit
import Tora.AST (Program)
import Tora.Parser
import Data.Either

----------------
-- TEST SUITE --
----------------

runParserTests = runTestTT testParser

testParser :: Test
testParser = TestList
  []

t1 :: ByteString
t1 = [tigerSrc| nil |]

t2 = displayAST . fromRight' $ runParser t1
