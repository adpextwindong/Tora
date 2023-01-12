module Tora.Parser.Tests.Util where

import Data.ByteString.Lazy.Char8 (ByteString)
import Tora.AST (Program)
import qualified Tora.Lexer as L
import Test.HUnit
import Data.Either
import Data.Bifunctor
import Tora.Parser

stdParserTest :: String -> ByteString -> (Program L.Range -> Bool ) -> Test
stdParserTest testName testInput testCase = TestCase $ do
  let output = runParser testInput
  assertBool testName $ fromRight False $ second testCase output

testShouldFail :: ByteString -> Bool
testShouldFail = isLeft . runParser
