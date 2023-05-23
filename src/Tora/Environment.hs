module Tora.Environment where

import Tora.AST

import qualified Data.Map as M
import Data.ByteString.Lazy.Char8 (ByteString)

data EnvEntry t = VarEntry t Bool -- Bool flag denotes readOnly forloop var
                | FunEntry [t] t

--Slow chained scopes
data Env t = EmptyEnv
           | LexicalScope {
               vEnv :: M.Map ByteString (EnvEntry t) -- Variables and function declarations
               ,tyScope :: M.Map ByteString t        -- Type Decls
               ,parentScope :: Env t                 -- Chained Parent Scope SLOW
           }

type Environment a = Env (Ty a)

--Used to introduce a new lexical scope
mkScope :: Environment a -> Environment a
mkScope env = LexicalScope M.empty M.empty env

varLookup :: Env b -> Name a -> Maybe (EnvEntry b)
varLookup EmptyEnv _ = Nothing
varLookup (LexicalScope s  _ p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> varLookup p name

varLocalLookup :: Env b -> Name a -> Maybe (EnvEntry b)
varLocalLookup EmptyEnv n = Nothing
varLocalLookup (LexicalScope s _ _) (Name _ n) = M.lookup n s

funLocalLookup :: Env b -> Name a -> Maybe (EnvEntry b)
funLocalLookup = varLocalLookup

funLookup = varLookup

typeLocalLookup :: Env b -> Name a -> Maybe b
typeLocalLookup EmptyEnv n = Nothing
typeLocalLookup (LexicalScope _ tS _) (Name _ n) = M.lookup n tS

insertVarScopeEnv :: Environment a -> Name a -> Ty a -> Bool -> Environment a
insertVarScopeEnv EmptyEnv (Name _ n) t readOnly = LexicalScope (M.singleton n (VarEntry t readOnly)) M.empty EmptyEnv
insertVarScopeEnv (LexicalScope vE tS p) (Name _ n) t readOnly = LexicalScope vE' tS p
  where vE' = M.insert n (VarEntry t readOnly) vE

insertFunScopeEnv :: Environment a -> Name a -> [Ty a] -> Ty a -> Environment a
insertFunScopeEnv EmptyEnv (Name _ n) argts t = LexicalScope (M.singleton n (FunEntry argts t)) M.empty EmptyEnv
insertFunScopeEnv (LexicalScope vE tS p) (Name _ n) argsts t = LexicalScope vE' tS p
  where vE' = M.insert n (FunEntry argsts t) vE


insertTyScopeEnv :: Environment a -> Name a -> Ty a -> Environment a
insertTyScopeEnv EmptyEnv (Name _ n) t = LexicalScope M.empty (M.singleton n t) EmptyEnv
insertTyScopeEnv (LexicalScope vE tS p) (Name _ n) t = LexicalScope vE tS' p
  where tS' = M.insert n t tS

typeLookup :: Env (Ty a) -> Name a -> Maybe (Ty a)
typeLookup _ (Name _ "int") = Just $ TigInt
typeLookup _ (Name _ "string") = Just $ TigString
typeLookup EmptyEnv _ = Nothing
typeLookup (LexicalScope _  s p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> typeLookup p name
