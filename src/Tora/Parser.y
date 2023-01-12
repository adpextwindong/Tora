{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Tora.Parser
  ( parseTiger
  , runParser
  , displayAST
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid (First (..))
import Data.Either
import Data.Either.Extra
import Data.Bifunctor

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
  ":="              { L.RangedToken (L.TVarDecEquals) _ }
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
               : function name '(' tyFields ')' optional(typeAnnotation) '=' expr
                  { FunDeclaration (L.rtRange $1 <-> info $8) $2 $4 $6 $8 }

typeAnnotation :: { Type L.Range }
               : ':' ty { $2 }

expr :: { Expr L.Range }
     : nil { NilExpr (L.rtRange $1) }

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

displayAST :: (Functor f) => f a -> f ()
displayAST = fmap (const ())

}
