{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Data.Unique (Unique, newUnique)
import Control.Monad.Except

import Debug.Trace

type TypeCheckM = ExceptT TypeError IO

data Ty a = TigInt
          | TigString
          | TigRecord [(Name a, Ty a)] (Maybe Unique)
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
               | NothingTypeInField --TODO test
               | NonUniqueRecordFieldName --TODO test
               | RecordFieldMismatchError
               | RecordFieldExprNameMismatch
               | RecordExprTyFieldMismatch
               | AnonymousTypeUsageError
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


assertTyE :: Eq a => Environment a -> Expr a -> Ty a -> TypeCheckM ()
assertTyE env expr t = do
  ty <- typeCheckE env expr
  if t == ty
  then return ()
  else throwError AssertTyError

traceTrick v = trace ("\n\nFIXME:\n" <> show (void v) <> "\n") False
traceTrick2 e t = trace ("\n\nFIXME:\n" <> show (void t) <> "\n" <> show (void e)) False

typeCheckProg :: (Show a, Eq a) => Program a -> IO (Either TypeError ())
typeCheckProg (ProgExpr _ e) = runExceptT $ void $ typeCheckE EmptyEnv e
typeCheckProg (ProgDecls _ decs) = runExceptT $ void $ typeCheckDecs EmptyEnv decs

typeCheckDecs :: (Show a, Eq a) => Environment a -> [Declaration a] -> TypeCheckM (Env (Ty a))
typeCheckDecs env [] = return env
typeCheckDecs env (d:ds) = do
  mTy <- typeCheckDec env d
  env' <- case mTy of
            Nothing -> return env
                                --Check Var Decl hasn't been made before
            Just (name, ty) -> if isNothing (varLocalLookup env name)
                               then return $ insertTyScopeEnv env name ty
                               else throwError VarTyDecShadowError
  typeCheckDecs env' ds

typeCheckDec :: (Show a, Eq a) => Environment a -> Declaration a -> TypeCheckM (Maybe (Name a,Ty a))
typeCheckDec env (TypeDeclaration info name ty) | isReservedTyName name = throwError ReservedBaseTyNameError
                                                | otherwise = typeCheckTyDec env name ty --TODO typeCheckTyDec completeness

typeCheckDec env (VarDeclaration _ name Nothing (NilExpr _)) = throwError RawVarNilDeclError
typeCheckDec env (VarDeclaration _ name Nothing e) = do
  tyE <- typeCheckE env e --TODO typeCheckE completeness
  return $ Just (name, tyE)

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ "int"))) e) = do
  tyE <- typeCheckE env e
  if tyE == TigInt
  then return $ Just (n,TigInt)
  else throwError TypeAliasMismatchError

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ "string"))) e) = do
  tyE <- typeCheckE env e
  if tyE == TigString
  then return $ Just (n,TigInt)
  else throwError TypeAliasMismatchError

--TODO record distinction scheme
typeCheckDec env decl@(VarDeclaration _ name t@(Just (TVar _ n@(Name _ tn))) e@(RecordInitExpr _ (Name _ rn) rfields)) = do
  case typeLookup env n of
    Nothing -> throwError AnonymousTypeUsageError
    Just t@(TigRecord tfields _) -> do --figure out about unique
      if tn == rn
      then do
        checkMatchingRecordConstructor env tfields rfields
        return $ Just (n, t)
      else throwError RecordFieldMismatchError
    --Just _ =
    --Just _ = throwError TypeAliasMismatchError --TODO {- type foo = int var bar : foo = baz { val = int } -}


typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ _))) e) = do --TODO TEST
  tyE <- typeCheckE env e
  case typeLookup env n of
    Nothing -> throwError MissingTypeNameAliasingError
    Just t' -> do
      assertTyE env e t'
      return $ Just (name, t')

--TODO more Type cases
typeCheckDec env decl | traceTrick decl = undefined

typeCheckDec _ _ = undefined --TODO!!
--typeCheckDec env (VarDeclaration _ name (Just t) (NilExpr _)) = TODO! CHECK T FOR RECORD TYPE SEE PAGE 516 EX1
--typeCheckDec env (VarDeclaration _ name (Just t) e) = TODO! CHECK T AGAINST ty of e, typeToTy for eq'ing
--
--typeCheckDec TODO VarDecl
--typeCheckDec TODO FunDecl

typeCheckTyDec :: Eq a => Environment a -> Name a -> Type a -> TypeCheckM (Maybe (Name a,Ty a))
typeCheckTyDec env name (TVar _ n@(Name _ s)) | s == "int"    = return $ Just (name, TigInt)
                                              | s == "string" = return $ Just (name, TigString)
                                              | otherwise = case typeLookup env n of
                                                              Nothing -> throwError MissingTypeNameAliasingError
                                                              Just t -> return $ Just (name, t)

typeCheckTyDec env name (TRecord _ fields) = do
  tys <- mapM (typeCheckField env) fields
  let fieldNames = (\(TyField _ (Name _ s) _) -> s) <$> fields
  let uniqueFieldNames = S.fromList fieldNames

  if length uniqueFieldNames == length fieldNames
  then do
    unid <- liftIO newUnique
    return $ Just (name, TigRecord tys (Just unid))
  else throwError NonUniqueRecordFieldName --TODO test case

typeCheckTyDec _ _ _ = undefined --TODO!!
--typeCheckTyDec TODO TRecord, TyField, Nil handling
--typeCheckTyDec TODO TArray

typeCheckField :: Eq a => Environment a -> TyField a -> TypeCheckM (Name a, Ty a)
typeCheckField env (TyField _ n t) = do
  p <- typeCheckTyDec env n t
  case p of
    Just p' -> return p'
    Nothing -> throwError NothingTypeInField --TODO test case for this path


isReservedTyName :: Name a -> Bool
isReservedTyName (Name a name) = name == "int" || name == "string"

typeCheckE :: Env (Ty a) -> Expr a -> TypeCheckM (Ty a)
typeCheckE _ (NilExpr _) = return TigNil
typeCheckE _ (IntLitExpr _ _) = return TigInt
typeCheckE _ (StringLitExpr _ _) = return TigString

typeCheckE env (RecordInitExpr _ n fields) = do
  tys <- mapM (typeCheckE env . snd) fields
  let fieldNames = fst <$> fields
  return $ TigRecord (zip fieldNames tys) Nothing

typeCheckE _ _ = undefined --TODO!!
--TODO typeCheck EXPR(..)

--TODO LetExpr creates a new scope for ty checking declarations

withScope :: Ord a => Environment a -> [Declaration a] -> Environment a
withScope env decs = undefined --TODO modify env

checkMatchingRecordConstructor :: (Eq a) => Environment a -> [(Name a, Ty a)] -> [(Name a, Expr a)] -> TypeCheckM ()
checkMatchingRecordConstructor env definitionTys exprFields =
  let ns (Name _ s) = s in
  if (ns . fst <$> definitionTys) == (ns . fst <$> exprFields)
  then forM_ (zip (snd <$> definitionTys) (snd <$> exprFields)) $
    \(tyDef, e) -> do
      eTy <- typeCheckE env e
      unless (tyDef == eTy) $ throwError RecordExprTyFieldMismatch
  else throwError RecordFieldExprNameMismatch




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
 -     - typeCheckTyDec completeness
 -
 -  - VarDeclaration
 -    - Nil Handling for record
 -    - Record and Array
 -
 -  - FunDeclaration
 -    -TODO
 -
 - Type
 -  - TVar
 -  - TRecord
 -  - TArray
 -
 - TyField

 - Expr
 - LValue
-}
