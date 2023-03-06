{-# LANGUAGE OverloadedStrings #-}
module Tora.TypeChecker where

import qualified Data.Map as M
import qualified Data.Set as S

import Tora.AST

import Control.Monad
import Control.Monad.Gen
import Control.Monad.Trans.Maybe
import Control.Applicative
import Prelude hiding (lookup)
import Data.ByteString.Lazy.Char8 (ByteString)
import Language.Haskell.TH (lookupTypeName)

data Ty a = TigInt
          | TigString
          | TigRecord [(Name a, Ty a)] (Name a) --we need a unique id here
          | TigArray (Ty a) (Name a)
          | TigNil
          | TigUnit
  deriving Eq

data TypeError = AssertTyError
               | ReservedBaseTyNameError
               | MissingTypeNameAliasingError
               deriving Show

--Slow chained scopes
data Env t = EmptyEnv
           | LexicalScope {
               varScope :: M.Map ByteString t
               ,funTypeScope :: M.Map ByteString t
               ,parentScope :: Env t
           }
type Environment a = Env (Ty a)

varLookup :: Env b -> Name a -> Maybe b
varLookup EmptyEnv _ = Nothing
varLookup (LexicalScope s  _ p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> varLookup p name

typeLookup :: Env b -> Name a -> Maybe b
typeLookup EmptyEnv _ = Nothing
typeLookup (LexicalScope _  s p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> typeLookup p name


--TODO this needs to branch correctly on the type spaces
insertTyEnv :: Environment a -> Name a -> Ty a -> Environment a
insertTyEnv = undefined --TODO!!

assertTyE :: Eq a => Environment a -> Expr b -> Ty a -> Either TypeError ()
assertTyE env expr t = do
  ty <- typeCheckE env expr
  if t == ty
  then return ()
  else Left AssertTyError

typeCheckProg :: Eq a => Program a -> Either TypeError ()
typeCheckProg (ProgExpr _ e) = void $ typeCheckE EmptyEnv e
typeCheckProg (ProgDecls _ decs) = void $ typeCheckDecs EmptyEnv decs

typeCheckDecs :: Eq a => Environment a -> [Declaration a] -> Either TypeError (Env (Ty a))
typeCheckDecs env [] = return env
typeCheckDecs env (d:ds) = do
  mTy <- typeCheckDec env d
  env' <- case mTy of
            Nothing -> Right env
            Just (name, ty) -> Right $ insertTyEnv env name ty
  typeCheckDecs env' ds

typeCheckDec :: Eq a => Environment a -> Declaration a -> Either TypeError (Maybe (Name a,Ty a))
typeCheckDec env (TypeDeclaration info name ty) | isReservedTyName name = Left ReservedBaseTyNameError
                                                | otherwise = typeCheckTyDec env name ty

typeCheckTyDec :: Eq a => Environment a -> Name a -> Type a -> Either TypeError (Maybe (Name a,Ty a))
typeCheckTyDec env name (TVar _ n@(Name _ s)) | s == "int"    = Right $ Just (name, TigInt)
                                              | s == "string" = Right $ Just (name, TigString)
                                              | otherwise = case typeLookup env n of
                                                              Nothing -> Left MissingTypeNameAliasingError
                                                              Just t -> Right $ Just (name, t)

isReservedTyName :: Name a -> Bool
isReservedTyName (Name a name) = name == "int" || name == "string"

typeCheckE :: Env (Ty a) -> Expr b -> Either TypeError (Ty a)
typeCheckE = undefined

--TODO LetExpr creates a new scope for ty checking declarations

withScope :: Ord a => Environment a -> [Declaration a] -> Environment a
withScope env decs = undefined --TODO modify env
