module Tora.TypeChecker where

import qualified Data.Map as M

import Tora.AST

import Control.Monad
import Control.Monad.Gen
import Control.Monad.Trans.Maybe
import Control.Applicative
import Prelude hiding (lookup)

data Ty a = TigInt
          | TigString
          | TigRecord [(Name a, Ty a)] (Name a) --we need a unique id here
          | TigArray (Ty a) (Name a)
          | TigNil
          | TigUnit
  deriving Eq

data TypeError = AssertTyError

--Slow chained scopes
data Env n t = EmptyEnv
             | LexicalScope {
                  varScope :: M.Map n t
                 ,funTypeScope :: M.Map n t
                 ,parentScope :: Env n t
             }
type Environment a = Env (Name a) (Ty a)

varLookup :: Ord a => Env a b -> a -> Maybe b
varLookup EmptyEnv _ = Nothing
varLookup (LexicalScope s  _ p) n = case M.lookup n s of
                                Just t -> Just t
                                Nothing -> varLookup p n

typeLookup :: Ord a => Env a b -> a -> Maybe b
typeLookup EmptyEnv _ = Nothing
typeLookup (LexicalScope _  s p) n = case M.lookup n s of
                                Just t -> Just t
                                Nothing -> typeLookup p n



assertTyE :: Ord a => Environment a -> Expr b -> Ty a -> Either TypeError ()
assertTyE env expr t = do
  ty <- typeCheckE env expr
  if t == ty
  then return ()
  else Left AssertTyError

typeCheckProg :: Program a -> Either TypeError ()
typeCheckProg (ProgExpr _ e) = void $ typeCheckE EmptyEnv e
typeCheckProg (ProgDecls _ decs) = void $ typeCheckDecs EmptyEnv decs

typeCheckDecs :: Environment a -> [Declaration a] -> Either TypeError (Env (Name a) (Ty a))
typeCheckDecs env [] = return env
typeCheckDecs env (d:ds) = undefined

typeCheckDec :: Env (Name a) (Ty a) -> [Declaration a] -> Either TypeError (Maybe (Name a,Ty a))
typeCheckDec = undefined

typeCheckE :: Env (Name a) (Ty a) -> Expr b -> Either TypeError (Ty a)
typeCheckE = undefined
