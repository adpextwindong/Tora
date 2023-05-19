{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Tora.TypeChecker where

import qualified Data.Set as S

import Tora.AST

--TODO clean env handling shit up
import Tora.Environment (Environment, Env(..), EnvEntry(..),
  varLocalLookup, typeLocalLookup, -- Just used for local shadowing checks
  insertVarScopeEnv, insertTyScopeEnv, -- This handling in particular
  typeLookup, varLookup,
  mkScope)

import Data.Maybe (isNothing)
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Unique (Unique, newUnique)
import Control.Monad.Except

import Debug.Trace

type TypeCheckM = ExceptT TypeError IO

data TypeError = AssertTyError
               | ReservedBaseTyNameError
               | MissingTypeNameAliasingError
               | RawVarNilDeclError
               | NothingTypeInField --TODO test
               | NonUniqueRecordFieldName --TODO test
               | VarDeclTypeMismatchError
               | RecordFieldMismatchError
               | RecordFieldExprNameMismatch
               | RecordExprTyFieldMismatch
               | RecordTypeMismatchError
               | AnonymousTypeUsageError
               | InvalidLValueNameError
               | UndeclaredRecordTypeUsageError
               | InvalidLValueBaseNameError
               | IfThenTypeMismatchError
               | InvalidIFECondTypeError
               | InvalidIFEBodyTypeError
               | InvalidWhileCondTypeError
               | InvalidWhileBodyTypeError
               | InvalidForStartEndTypeError
               | InvalidForBodyTypeError
               | VarFunDecShadowError
               | TypeDecShadowError
               | TypeLiteralMismatchError
               deriving (Show, Eq)

assertTyE :: (Show a, Eq a) => Environment a -> Expr a -> Ty a -> TypeCheckM ()
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
  (name, ty) <- typeCheckDec env d

  env' <- case d of
    TypeDeclaration _ _ _ -> do --insert into typescope
      if isNothing (typeLocalLookup env name)
      then return $ insertTyScopeEnv env name ty
      else throwError TypeDecShadowError

    VarDeclaration _ _ _ _ -> do --insert into varFun scope
      if isNothing (varLocalLookup env name)
      then return $ insertVarScopeEnv env name ty
      else throwError VarFunDecShadowError

    FunDeclaration _ _ _ _ _ -> undefined


  {-
  env' <- case mTy of
            Nothing -> return env
                                --Check Var Decl hasn't been made before
            Just (name, ty) -> if isNothing (varLocalLookup env name)
                               then return $ insertTyScopeEnv env name ty
                               else throwError VarTyDecShadowError
  -}
  typeCheckDecs env' ds

typeCheckDec :: (Show a, Eq a) => Environment a -> Declaration a -> TypeCheckM (Name a,Ty a)
typeCheckDec env (TypeDeclaration info name ty) | isReservedTyName name = throwError ReservedBaseTyNameError
                                                | otherwise = typeCheckTyDec env name ty --TODO typeCheckTyDec completeness

typeCheckDec env (VarDeclaration _ name Nothing (NilExpr _)) = throwError RawVarNilDeclError
typeCheckDec env (VarDeclaration _ name Nothing e) = do
  tyE <- typeCheckE env e --TODO typeCheckE completeness
  return $ (name, tyE)

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ "int"))) e) = do
  tyE <- typeCheckE env e
  if tyE == TigInt
  then return $ (n,TigInt)
  else throwError TypeLiteralMismatchError

typeCheckDec env (VarDeclaration _ name (Just (TVar _ n@(Name _ "string"))) e) = do
  tyE <- typeCheckE env e
  if tyE == TigString
  then return $ (n,TigInt)
  else throwError TypeLiteralMismatchError

typeCheckDec env decl@(VarDeclaration _ name t@(Just (TVar _ nt@(Name _ tn))) e@(RecordInitExpr _ rt@(Name _ rn) rfields)) = do
  case typeLookup env nt of
    Nothing -> throwError AnonymousTypeUsageError
    Just t@(TigRecord _ _) -> do
      if tn == rn
      then do
        checkMatchingRecUnid env t (typeLookup env rt)
        typeCheckE env e
        return $ (nt, t)
      else throwError VarDeclTypeMismatchError
    Just _ -> throwError TypeLiteralMismatchError
  where
    checkMatchingRecUnid :: Environment a -> Ty a -> Maybe (Ty a) -> TypeCheckM ()
    checkMatchingRecUnid env (TigRecord _ u) (Just (TigRecord _ u')) =
      unless (u == u') $ throwError RecordTypeMismatchError


typeCheckDec env (VarDeclaration _ name (Just (TVar _ tn@(Name _ _))) e) = do --TODO TEST
  tyE <- typeCheckE env e
  case typeLookup env tn of
    Nothing -> throwError MissingTypeNameAliasingError
    Just t' -> do
      assertTyE env e t'
      return $ (name, t')

--TODO more Type cases
typeCheckDec env decl | traceTrick decl = undefined

typeCheckDec _ _ = undefined --TODO!!
--typeCheckDec env (VarDeclaration _ name (Just t) (NilExpr _)) = TODO! CHECK T FOR RECORD TYPE SEE PAGE 516 EX1
--typeCheckDec env (VarDeclaration _ name (Just t) e) = TODO! CHECK T AGAINST ty of e, typeToTy for eq'ing
--
--typeCheckDec TODO VarDecl
--typeCheckDec TODO FunDecl


--TODO was this maybe necessary???
typeCheckTyDec :: Eq a => Environment a -> Name a -> Type a -> TypeCheckM (Name a,Ty a)
typeCheckTyDec env name (TVar _ n@(Name _ s)) | s == "int"    = return $ (name, TigInt)
                                              | s == "string" = return $ (name, TigString)
                                              | otherwise = case typeLookup env n of
                                                              Nothing -> throwError MissingTypeNameAliasingError
                                                              Just t -> return $ (name, t)

typeCheckTyDec env name (TRecord _ fields) = do
  tys <- mapM (typeCheckField env) fields
  let fieldNames = (\(TyField _ (Name _ s) _) -> s) <$> fields
  let uniqueFieldNames = S.fromList fieldNames

  if length uniqueFieldNames == length fieldNames
  then do
    unid <- liftIO newUnique
    return $ (name, TigRecord tys unid)
  else throwError NonUniqueRecordFieldName --TODO test case

typeCheckTyDec _ _ _ = undefined --TODO!!
--typeCheckTyDec TODO TRecord Nil Handling, TyField, Nil handling
--typeCheckTyDec TODO TArray

typeCheckField :: Eq a => Environment a -> TyField a -> TypeCheckM (Name a, Ty a)
typeCheckField env (TyField _ n t) = typeCheckTyDec env n t

isReservedTyName :: Name a -> Bool
isReservedTyName (Name a name) = name == "int" || name == "string"

typeCheckE :: (Show a, Eq a) => Env (Ty a) -> Expr a -> TypeCheckM (Ty a)
typeCheckE _ (NilExpr _) = return TigNil
typeCheckE _ (IntLitExpr _ _) = return TigInt
typeCheckE _ (StringLitExpr _ _) = return TigString
typeCheckE env (ExprSeq _ es) = do
  tys <- mapM (typeCheckE env) es
  return . head . reverse $ tys

typeCheckE env (RecordInitExpr _ n rfields) = do
  case typeLookup env n of
    Nothing -> throwError UndeclaredRecordTypeUsageError -- TODO TEST [tigerSrc| var x = foo { val = 1 } |]
    Just (t@(TigRecord tfields unid)) -> do
       checkMatchingRecConStructure env tfields rfields
       return $ t
    _ -> throwError AssertTyError -- TODO TEST [tigerSrc| var x : int = rec { baz = 1 } |]

typeCheckE env (LetExpr _ decs e) = do
  env' <- typeCheckDecs (mkScope env) decs --TODO scope tests
  ty <- typeCheckE env' e
  return ty

--TENTATIVE
typeCheckE env (LValueExpr _ (LValueBase _ n)) = do
  case varLookup env n of
    Just (VarEntry t) -> return t
    Just (FunEntry _ t) -> return t
    Nothing -> throwError $ InvalidLValueBaseNameError

typeCheckE _ (NoValueExpr _) = return TigNoValue

typeCheckE env (IFThenExpr _ e e') = do
  t <- typeCheckE env e
  case t of
    TigInt -> do
      t' <- typeCheckE env e'
      case t' of
        TigNoValue -> return TigNoValue
        _ -> throwError IfThenTypeMismatchError
    _ -> throwError IfThenTypeMismatchError

typeCheckE env (IFThenElseExpr _ cond e e') = do
  thead <- typeCheckE env cond
  case thead of
    TigInt -> do
      t <- typeCheckE env e
      t' <- typeCheckE env e'
      if t == t'
      then return t
      else throwError InvalidIFEBodyTypeError
    _ -> throwError InvalidIFECondTypeError

typeCheckE env (WhileExpr _ cond e) = do
  tcond <- typeCheckE env cond
  case tcond of
    TigInt -> do
      t <- typeCheckE env e
      case t of
        TigNoValue -> return TigNoValue
        _ -> throwError InvalidWhileBodyTypeError
    _ -> throwError InvalidWhileCondTypeError

--TODO body may not assign to forVarName
typeCheckE env (ForExpr lxr_range forVarName forVarEStart forVarEEnd body) = do
  tstart <- typeCheckE env forVarEStart
  tend <- typeCheckE env forVarEEnd

  when (tstart /= TigInt || tend /= TigInt) $ throwError InvalidForStartEndTypeError

  --HACK HUGE PITFALL this distinction between var scope and type scope is brittle as is rn
  --TODO scope tests
  let env' = insertVarScopeEnv (mkScope env) forVarName TigInt
  tbody <- typeCheckE env' body

  case tbody of
    TigNoValue -> return TigNoValue
    _ -> throwError InvalidForBodyTypeError


typeCheckE _ w | traceTrick w = undefined
--TODO typeCheck EXPR(..)

--TODO LetExpr creates a new scope for ty checking declarations

--TODO make sure this gets used.

checkMatchingRecConStructure :: (Show a, Eq a) => Environment a -> [(Name a, Ty a)] -> [(Name a, Expr a)] -> TypeCheckM ()
checkMatchingRecConStructure env definitionTys exprFields =
  let ns (Name _ s) = s in
  if (ns . fst <$> definitionTys) == (ns . fst <$> exprFields)
  then forM_ (zip (snd <$> definitionTys) (snd <$> exprFields)) $
    \(tyDef, e) -> do
      eTy <- typeCheckE env e
      unless (tyDef == eTy) $ throwError RecordExprTyFieldMismatch
  else throwError RecordFieldExprNameMismatch




--TODO walk through the AST types and make tests
--TODO nil belongs to any record type
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
