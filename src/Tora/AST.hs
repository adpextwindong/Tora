{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Tora.AST where

import Data.ByteString.Lazy.Char8 (ByteString)
---------
-- AST --
---------

data Declaration a
  = TypeDeclaration a (Name a) (Type a)
  | VarDeclaration a (Name a) (Maybe (Type a)) (Expr a)
  | FunDeclaration a (Name a) [TyField a] (Maybe (Type a)) (Expr a)
  deriving (Functor, Foldable, Show)

data Name a
  = Name a ByteString
  deriving (Functor, Foldable, Show, Eq, Ord)

data Type a
  = TVar a (Name a)
  | TRecord a [TyField a]
  | TArray a (Type a)
  deriving (Functor, Foldable, Show)

data TyField a
  = TyField a (Name a) (Type a)
  deriving (Functor, Foldable, Show)

data Expr a
  = NilExpr a
  | IntLitExpr a Int
  | StringLitExpr a ByteString
  | ExprSeq a [Expr a]
  | LetExpr a [Declaration a] (Expr a) -- | Uses an ExprSeq
  | IFThenExpr a (Expr a) (Expr a)
  | IFThenElseExpr a (Expr a) (Expr a) (Expr a)
  | WhileExpr a (Expr a) (Expr a)
  | ForExpr a (Name a) (Expr a) (Expr a) (Expr a)
  | BreakExpr a
  | FunCallExpr a (Name a) [Expr a]
  | ArrayInitExpr a (Name a) (Expr a) (Expr a)
  | RecordInitExpr a (Name a) [(Name a, Expr a)]
  | NoValueExpr a
  | BinOpExpr a (Expr a) (Operator a) (Expr a)
  | UnaryNegate a (Expr a)
  | LValueExpr a (LValue a)
  | AssignmentExpr a (LValue a) (Expr a)
  deriving (Functor, Foldable, Show)

data LValue a
  = LValueBase a (Name a)
  | LValueDot a (LValue a) (Name a)
  | LValueArray a (LValue a) (Expr a)
  deriving (Functor, Foldable, Show)

data Operator a
  = BoolAndOp a
  | BoolOrOp a
  | MulOp a
  | DivOp a
  | PlusOp a
  | MinusOp a
  | EqualOp a
  | NEqualOp a
  | GTOp a
  | LTOp a
  | GTEOp a
  | LTEOp a
  deriving (Functor, Foldable, Show)

data Program a
  = ProgExpr a (Expr a)
  | ProgDecls a [Declaration a]
  deriving (Functor, Foldable, Show)
