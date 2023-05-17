module Tora.Environment where

import Tora.AST

import qualified Data.Map as M
import Data.ByteString.Lazy.Char8 (ByteString)

data EnvEntry t = VarEntry t
                | FunEntry [t] t

--Slow chained scopes
data Env t = EmptyEnv
           | LexicalScope {
               vEnv :: M.Map ByteString (EnvEntry t) -- Variables and function declarations
               ,tyScope :: M.Map ByteString t        -- Type Decls
               ,parentScope :: Env t                 -- Chained Parent Scope SLOW
           }

type Environment a = Env (Ty a)

varLookup :: Env b -> Name a -> Maybe (EnvEntry b)
varLookup EmptyEnv _ = Nothing
varLookup (LexicalScope s  _ p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> varLookup p name

varLocalLookup :: Env b -> Name a -> Maybe (EnvEntry b)
varLocalLookup EmptyEnv _ = Nothing
varLocalLookup (LexicalScope s _ _) (Name _ n) = M.lookup n s

insertVarScopeEnv :: Environment a -> Name a -> Ty a -> Environment a
insertVarScopeEnv EmptyEnv (Name _ n) t = LexicalScope (M.singleton n (VarEntry t)) M.empty EmptyEnv
insertVarScopeEnv (LexicalScope vE tS p) (Name _ n) t = LexicalScope vE' tS p
  where vE' = M.insert n (VarEntry t) vE

insertTyScopeEnv :: Environment a -> Name a -> Ty a -> Environment a
insertTyScopeEnv EmptyEnv (Name _ n) t = LexicalScope M.empty (M.singleton n t) EmptyEnv
insertTyScopeEnv (LexicalScope vE tS p) (Name _ n) t = LexicalScope vE tS' p
  where tS' = M.insert n t tS

typeLookup :: Env b -> Name a -> Maybe b
typeLookup EmptyEnv _ = Nothing
typeLookup (LexicalScope _  s p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> typeLookup p name
