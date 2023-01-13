{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Tora.Parser.Tests.ExprTests (
    testExpr
  ) where

import Test.HUnit
import Tora.QQ
import Tora.Parser.Tests.Util (stdParserTest)
import Tora.AST
import Data.ByteString.Lazy.Char8 (ByteString)

testExpr = TestList
         [testNilExpr]

testNilExpr = stdParserTest "Nil Expr"
              [tigerSrc| nil |]
              (\case
                (ProgExpr _ (NilExpr _)) -> True
                _ -> False)
