{-# LANGUAGE OverloadedStrings #-}
module Tora.TypeChecker where

import qualified Data.Map as M
import qualified Data.Set as S

import Tora.AST

import Data.Maybe (isNothing)
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Trans.Maybe
import Control.Applicative
import Prelude hiding (lookup)
import Data.ByteString.Lazy.Char8 (ByteString)

import Debug.Trace

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
               | VarTyDecShadowError
               | RawVarNilDeclError
               | TypeAliasMismatchError
               deriving (Show, Eq)

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

insertTyScopeEnv :: Environment a -> Name a -> Ty a -> Environment a
insertTyScopeEnv EmptyEnv (Name _ n) t = LexicalScope M.empty (M.singleton n t) EmptyEnv
insertTyScopeEnv (LexicalScope vE tS p) (Name _ n) t = LexicalScope vE tS' p
  where tS' = M.insert n t tS

typeLookup :: Env b -> Name a -> Maybe b
typeLookup EmptyEnv _ = Nothing
typeLookup (LexicalScope _  s p) name@(Name _ n) = case M.lookup n s of
                                                Just t -> Just t
                                                Nothing -> typeLookup p name


assertTyE :: Eq a => Environment a -> Expr b -> Ty a -> Either TypeError ()
assertTyE env expr t = do
  ty <- typeCheckE env expr
  if t == ty
  then return ()
  else Left AssertTyError

traceTrick v = trace ("\n\nFIXME:\n" <> show (void v) <> "\n") False

typeCheckProg :: (Show a, Eq a) => Program a -> Either TypeError ()
typeCheckProg (ProgExpr _ e) = void $ typeCheckE EmptyEnv e
typeCheckProg (ProgDecls _ decs) = void $ typeCheckDecs EmptyEnv decs

typeCheckDecs :: (Show a, Eq a) => Environment a -> [Declaration a] -> Either TypeError (Env (Ty a))
typeCheckDecs env [] = return env
typeCheckDecs env (d:ds) = do
  mTy <- typeCheckDec env d
  env' <- case mTy of
            Nothing -> Right env
                                --Check Var Decl hasn't been made before
            Just (name, ty) -> if isNothing (varLocalLookup env name)
                               then Right $ insertTyScopeEnv env name ty
                               else Left VarTyDecShadowError
  typeCheckDecs env' ds

typeCheckDec :: (Show a, Eq a) => Environment a -> Declaration a -> Either TypeError (Maybe (Name a,Ty a))
typeCheckDec env (TypeDeclaration info name ty) | isReservedTyName name = Left ReservedBaseTyNameError
                                                | otherwise = typeCheckTyDec env name ty
typeCheckDec env (VarDeclaration _ name Nothing (NilExpr _)) = Left RawVarNilDeclError
typeCheckDec env (VarDeclaration _ name Nothing e) = do
  tyE <- typeCheckE env e
  Right $ Just (name, tyE)

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ "int"))) e) = do
  tyE <- typeCheckE env e
  if tyE == TigInt
  then Right $ Just (n,TigInt)
  else Left TypeAliasMismatchError

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ "string"))) e) = do
  tyE <- typeCheckE env e
  if tyE == TigString
  then Right $ Just (n,TigInt)
  else Left TypeAliasMismatchError

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ _))) e) = do
  tyE <- typeCheckE env e
  case typeLookup env n of
    Nothing -> Left MissingTypeNameAliasingError
    Just t' -> do
      assertTyE env e t'
      Right $ Just (name, t')

--TODO more Type cases
typeCheckDec env decl | traceTrick decl = undefined

typeCheckDec _ _ = undefined --TODO!!
--typeCheckDec env (VarDeclaration _ name (Just t) (NilExpr _)) = TODO! CHECK T FOR RECORD TYPE SEE PAGE 516 EX1
--typeCheckDec env (VarDeclaration _ name (Just t) e) = TODO! CHECK T AGAINST ty of e, typeToTy for eq'ing
--
--typeCheckDec TODO VarDecl
--typeCheckDec TODO FunDecl

typeCheckTyDec :: Eq a => Environment a -> Name a -> Type a -> Either TypeError (Maybe (Name a,Ty a))
typeCheckTyDec env name (TVar _ n@(Name _ s)) | s == "int"    = Right $ Just (name, TigInt)
                                              | s == "string" = Right $ Just (name, TigString)
                                              | otherwise = case typeLookup env n of
                                                              Nothing -> Left MissingTypeNameAliasingError
                                                              Just t -> Right $ Just (name, t)
typeCheckTyDec _ _ _ = undefined --TODO!!
--typeCheckTyDec TODO TRecord, TyField, Nil handling
--typeCheckTyDec TODO TArray

isReservedTyName :: Name a -> Bool
isReservedTyName (Name a name) = name == "int" || name == "string"

typeCheckE :: Env (Ty a) -> Expr b -> Either TypeError (Ty a)
typeCheckE _ (NilExpr _) = Right TigNil
typeCheckE _ (IntLitExpr _ _) = Right TigInt
typeCheckE _ (StringLitExpr _ _) = Right TigString
typeCheckE _ _ = undefined --TODO!!
--TODO typeCheck EXPR(..)

--TODO LetExpr creates a new scope for ty checking declarations

withScope :: Ord a => Environment a -> [Declaration a] -> Environment a
withScope env decs = undefined --TODO modify env

--TODO walk through the AST types and make tests
--TODO nil belongs to any record type
--TODO record type uniqueness
--TODO array type uniqueness
--TODO recurisve functions need to be typechecked carefully
--SKIP? Mutually recursive functions
--Nesting of break statements
--

{-
 -
 - AST TODO
 - Declaration
 -  - TypeDeclaration
 -  - VarDeclaration
 -  - FunDeclaration
 - Type
 -  - TVar
 -  - TRecord
 -  - TArray
 -
 - TyField

 - Expr
 - LValue
-}
