{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module Tora.TypeChecker where

import qualified Data.Set as S

import Tora.AST

--TODO clean env handling shit up
import Tora.Environment (Environment, Env(..), EnvEntry(..),
  varLocalLookup, funLocalLookup , typeLocalLookup, -- Just used for local shadowing checks
  insertVarScopeEnv, insertTyScopeEnv, -- This handling in particular
  insertFunScopeEnv,
  retainTypeScopeBareVars,
  typeLookup, varLookup, funLookup,
  mkScope)

import Data.Maybe (isNothing)
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Unique (Unique, newUnique)
import Control.Monad.Except

import Debug.Trace

type TypeCheckM = ExceptT TypeError IO

--Yeah I regret paramertizing the ast
type Bullshit a = (Eq a)

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
               | FnBodyHeadTypeMismatchError
               | BinOpTypeMismatch
               | MissingFunctionNameError --TODO test
               | CallingVarAsFunError --TODO test
               | FunCallArgTypeMismatchError --TODO test
               | InvalidArrayInitTypeError
               | UndefinedBinOpApplicationError
               | UndefinedUnaryNegateApplicationError
               | RecordWithMultipleMatchesError
               | RecordAccessOnNonRecordTypeError
               | NonIntegerArraySubscriptError
               | AssignmentTypeMismatchError
               | WriteAgainstReadOnlyLValueError
               deriving (Show, Eq)

assertTyE :: (Bullshit a) => Environment a -> Expr a -> Ty a -> TypeCheckM ()
assertTyE env expr t = do
  ty <- typeCheckE env expr
  if t == ty
  then return ()
  else throwError AssertTyError

traceTrick v = trace ("\n\nFIXME:\n" <> show (void v) <> "\n") False
traceTrick2 e t = trace ("\n\nFIXME:\n" <> show (void t) <> "\n" <> show (void e)) False

typeCheckProg :: (Bullshit a) => Program a -> IO (Either TypeError ())
typeCheckProg (ProgExpr _ e) = runExceptT $ void $ typeCheckE EmptyEnv e
typeCheckProg (ProgDecls _ decs) = runExceptT $ void $ typeCheckDecs EmptyEnv decs

consecTypedFns ds = takeWhile (\case { FunDeclaration _ _ _ (Just t) _ -> True; _ -> False }) ds

typeCheckDecs :: (Bullshit a) => Environment a -> [Declaration a] -> TypeCheckM (Env (Ty a))
typeCheckDecs env [] = return env
typeCheckDecs env ds | (length (consecTypedFns ds)) > 1 = do
  let typedfns = consecTypedFns ds
      restdecs = drop (length typedfns) ds
      ins (fn, ats, t) e = insertFunScopeEnv e fn ats t

  heads <- forM typedfns
    (\(FunDeclaration _ fn argFullTypes (Just fulltype) _) -> do
      argstys <- mapM ((fmap snd) . (lookupArgTyField env)) argFullTypes
      ty <- typeToTy env fulltype

      if isNothing (funLocalLookup env fn)
      then return $ (fn, argstys, ty)
      else throwError VarFunDecShadowError
    )

  let env' = foldr ins env heads

  forM_ typedfns
    (\f@(FunDeclaration _ fn argFullTypes (Just fulltype) _) -> typeCheckDec env' f)

  typeCheckDecs env' restdecs

typeCheckDecs env (d:ds) = do
  env' <- case d of
    TypeDeclaration _ name _ -> do --insert into typescope

      --TODO hoist this into a consecTyDecs and tombstone the mutually recursive types at once
      let envTOMB = insertTyScopeEnv env name (TigTOMBSTONE name)
      (name, ty) <- typeCheckDec envTOMB d
      if isNothing (typeLocalLookup env name)
      then return $ insertTyScopeEnv env name ty
      else throwError TypeDecShadowError

    VarDeclaration _ varName  _ _ -> do --insert into varFun scope
      (tn, ty) <- typeCheckDec env d
      if isNothing (varLocalLookup env tn)
      then return $ insertVarScopeEnv env varName ty False
      else throwError VarFunDecShadowError

    FunDeclaration _ fn argFullTypes mt _ -> do
      argstys <- mapM ((fmap snd) . (lookupArgTyField env)) argFullTypes

      let fnEnv = retainTypeScopeBareVars env
      env'' <- case mt of
                    Nothing -> return fnEnv
                    Just fulltype -> do
                      ty <- typeToTy env fulltype
                      return $ insertFunScopeEnv fnEnv fn argstys ty

      if isNothing (funLocalLookup env fn) --TODO fun shadowing var/fun test
      then
        do
          (name, ty) <- typeCheckDec env'' d
          return $ insertFunScopeEnv env'' fn argstys ty
      else throwError VarFunDecShadowError

  typeCheckDecs env' ds

typeCheckDec :: Bullshit a => Environment a -> Declaration a -> TypeCheckM (Name a,Ty a)
typeCheckDec env (TypeDeclaration info name ty) | isReservedTyName name = throwError ReservedBaseTyNameError
                                                | otherwise = typeCheckTyDec env name ty

typeCheckDec env (VarDeclaration _ name Nothing (NilExpr _)) = throwError RawVarNilDeclError
typeCheckDec env (VarDeclaration _ name Nothing e) = do
  tyE <- typeCheckE env e
  return $ (name, tyE)

--typeCheckDec env (VarDeclaration _ name (Just t) (NilExpr _)) = TODO! CHECK T FOR RECORD TYPE SEE PAGE 516 EX1
--
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
      case (t', tyE) of
        (TigRecord _ _ , TigNil) -> return (name, t')
        _ ->  do
          assertTyE env e t'
          return $ (name, t')

typeCheckDec env (FunDeclaration _ name fields Nothing e) = do --Untyped function
  env' <- mkFnScope env fields
  t <- typeCheckE env' e
  return (name, t)

typeCheckDec env (FunDeclaration _ name fields (Just t) e) = do
  t' <- typeToTy env t
  env' <- mkFnScope env fields
  t'' <- typeCheckE env' e
  if t' /= t''
  then throwError FnBodyHeadTypeMismatchError
  else return (name, t')

--TODO more Type cases
typeCheckDec env decl | traceTrick decl = undefined

typeCheckDec _ _ = undefined --TODO!!

lookupArgTyField :: Environment a -> TyField a -> TypeCheckM (Name a, Ty a)
lookupArgTyField env (TyField _ n t) = do
  ty <- typeToTy env t
  return (n, ty)

--TODO rename this to typeToTy, this just resolves Types to Tys in an environment
typeToTy :: Environment a -> Type a -> TypeCheckM (Ty a)
typeToTy env (TVar _ n@(Name _ s)) | s == "int"    = return TigInt
                                      | s == "string" = return TigString
                                      | otherwise = case typeLookup env n of
                                                      Nothing -> throwError MissingTypeNameAliasingError
                                                      Just t -> return t
typeToTy _ _ = undefined --TODO

mkFnScope :: Environment a -> [TyField a] -> TypeCheckM (Environment a)
mkFnScope env fields = do
  let ins (n,t) e = insertVarScopeEnv e n t False
  fieldsTyP <- forM fields $ do
    (\(TyField _ n fulltype) -> do
      t <- typeToTy env fulltype
      return (n,t))
  return $ foldr ins (mkScope env) fieldsTyP



typeCheckTyDec :: Eq a => Environment a -> Name a -> Type a -> TypeCheckM (Name a,Ty a)
typeCheckTyDec env name t@(TVar _ _) = do
  t' <- typeToTy env t
  return (name, t')

typeCheckTyDec env name (TRecord _ fields) = do
  tys <- mapM (typeCheckField env) fields
  let fieldNames = (\(TyField _ (Name _ s) _) -> s) <$> fields
  let uniqueFieldNames = S.fromList fieldNames

  if length uniqueFieldNames == length fieldNames
  then do
    unid <- liftIO newUnique
    return $ (name, TigRecord tys unid)
  else throwError NonUniqueRecordFieldName --TODO test case

typeCheckTyDec env name (TArray _ fulltype) = do
  (_,ty) <- typeCheckTyDec env name fulltype
  return (name, TigArray ty)

typeCheckField :: Eq a => Environment a -> TyField a -> TypeCheckM (Name a, Ty a)
typeCheckField env (TyField _ n t) = typeCheckTyDec env n t

isReservedTyName :: Name a -> Bool
isReservedTyName (Name a name) = name == "int" || name == "string"

typeCheckE :: (Bullshit a) => Env (Ty a) -> Expr a -> TypeCheckM (Ty a)
typeCheckE _ (NilExpr _) = return TigNil
typeCheckE _ (IntLitExpr _ _) = return TigInt
typeCheckE _ (StringLitExpr _ _) = return TigString

typeCheckE env (ExprSeq _ es) = do
  tys <- mapM (typeCheckE env) es
  return . head . reverse $ tys

typeCheckE env (LetExpr _ decs e) = do
  env' <- typeCheckDecs (mkScope env) decs --TODO scope tests
  ty <- typeCheckE env' e
  return ty

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

typeCheckE env (ForExpr lxr_range forVarName forVarEStart forVarEEnd body) = do
  tstart <- typeCheckE env forVarEStart
  tend <- typeCheckE env forVarEEnd

  when (tstart /= TigInt || tend /= TigInt) $ throwError InvalidForStartEndTypeError

  let env' = insertVarScopeEnv (mkScope env) forVarName TigInt True
  tbody <- typeCheckE env' body

  case tbody of
    TigNoValue -> return TigNoValue
    _ -> throwError InvalidForBodyTypeError

--TODO check that break is contained by for/while, might be another pass idk
typeCheckE _ (BreakExpr _) = return $ TigNoValue

typeCheckE env (FunCallExpr _ fn args) = do
  case funLookup env fn of
    Nothing -> throwError MissingFunctionNameError --TODO test
    Just (VarEntry _ _) -> throwError CallingVarAsFunError --TODO test
    Just (FunEntry argts t) -> do
      etys <- mapM (typeCheckE env) args

      if all (\p@(ft, et) ->
        case p of
          (TigRecord _ _, TigNil) -> True
          _ -> ft == et ) (zip argts etys)

      then return t
      else throwError FunCallArgTypeMismatchError

typeCheckE env (ArrayInitExpr _ typename initCount initValue) = do
  case typeLookup env typename of
    Nothing -> throwError MissingTypeNameAliasingError
    (Just t) -> do
      tCount <- typeCheckE env initCount
      t' <- typeCheckE env initValue
      let tyMatch = case t of
                      TigArray ty -> ty == t'
                      tym -> tym == t

      if tCount == TigInt && (tyMatch || t' == TigNil)
      then return $ TigArray t'
      else throwError InvalidArrayInitTypeError

typeCheckE env (RecordInitExpr _ n rfields) = do
  case typeLookup env n of
    Nothing -> throwError UndeclaredRecordTypeUsageError -- TODO TEST [tigerSrc| var x = foo { val = 1 } |]
    Just (t@(TigRecord tfields unid)) -> do
       checkMatchingRecConStructure env tfields rfields
       return $ t
    _ -> throwError AssertTyError -- TODO TEST [tigerSrc| var x : int = rec { baz = 1 } |]

typeCheckE _ (NoValueExpr _) = return TigNoValue

typeCheckE env (BinOpExpr _ e op e') = do --TODO
  t <- typeCheckE env e
  t' <- typeCheckE env e'
  if t /= t' && (t /= TigNil) && (t' /= TigNil)
  then throwError BinOpTypeMismatch
  else checkBinOp op t t'

typeCheckE env (UnaryNegate _ e) = do
  t <- typeCheckE env e
  if t == TigInt
  then return TigInt
  else throwError UndefinedUnaryNegateApplicationError

typeCheckE env (LValueExpr _ (LValueBase _ n)) = do
  case varLookup env n of
    Just (VarEntry t _) -> return t
    Nothing -> throwError InvalidLValueBaseNameError

typeCheckE env (LValueExpr a (LValueDot _ lvalue (Name _ accessor))) = do
  ty <- typeCheckE env (LValueExpr a lvalue)
  case ty of
    TigRecord nts unid -> do
      let matches = filter (\(Name _ n,t) -> n == accessor) nts
      if length matches == 1 --there should only be one matching field
      then return $ head (fmap snd matches)
      else throwError RecordWithMultipleMatchesError
    _ -> throwError RecordAccessOnNonRecordTypeError

typeCheckE env (LValueExpr _ (LValueArray a lvalue e)) = do
  te <- typeCheckE env e
  case te of
    TigInt -> do
      t <- typeCheckE env (LValueExpr a lvalue)
      return t
    _ -> throwError NonIntegerArraySubscriptError

typeCheckE env (AssignmentExpr a lvalue e) = do
  let isReadOnlyLValue = (\case
                            LValueBase _ n ->
                              case varLookup env n of
                                Just (VarEntry _ True) -> True
                                _ -> False
                            _ -> False)
  when (isReadOnlyLValue lvalue) $ throwError WriteAgainstReadOnlyLValueError

  ty <- typeCheckE env (LValueExpr a lvalue)
  t <- typeCheckE env e
  case ty of
    TigRecord _ _ ->
      if t == TigNil || ty == t
      then return TigNoValue
      else throwError AssignmentTypeMismatchError
    TigArray ty' ->
      if ty' == t
      then return TigNoValue
      else throwError AssignmentTypeMismatchError
    _ ->
      if ty == t
      then return TigNoValue --TODO test this
      else throwError AssignmentTypeMismatchError

checkBinOp :: Operator a -> Ty a -> Ty a -> TypeCheckM (Ty a)
checkBinOp (BoolAndOp _) TigInt TigInt = return TigInt
checkBinOp (BoolOrOp _) TigInt TigInt = return TigInt
checkBinOp (MulOp _) TigInt TigInt = return TigInt
checkBinOp (DivOp _) TigInt TigInt = return TigInt
checkBinOp (PlusOp _) TigInt TigInt = return TigInt
checkBinOp (PlusOp _) TigString TigString = return TigString
checkBinOp (MinusOp _) TigInt TigInt = return TigInt
checkBinOp (NEqualOp _) t@(TigRecord _ _) TigNil = return TigInt --NEQ returns a boolean for IFT/IFE
checkBinOp (NEqualOp _) TigNil t@(TigRecord _ _) = return TigInt --NEQ returns a boolean for IFT/IFE

checkBinOp (EqualOp _) t@(TigRecord _ _) TigNil = return TigInt --EQ returns a boolean for IFT/IFE
checkBinOp (EqualOp _) TigNil t@(TigRecord _ _) = return TigInt --EQ returns a boolean for IFT/IFE
checkBinOp (EqualOp _) t t' = return t
checkBinOp (NEqualOp _) t t' = return t
checkBinOp (GTOp _) TigInt TigInt = return TigInt
checkBinOp (LTOp _) TigInt TigInt = return TigInt
checkBinOp (GTEOp _) TigInt TigInt = return TigInt
checkBinOp (LTEOp _) TigInt TigInt = return TigInt
checkBinOp _ _ _ = throwError UndefinedBinOpApplicationError

checkMatchingRecConStructure :: Bullshit a => Environment a -> [(Name a, Ty a)] -> [(Name a, Expr a)] -> TypeCheckM ()
checkMatchingRecConStructure env definitionTys exprFields =
  let ns (Name _ s) = s in
  if (ns . fst <$> definitionTys) == (ns . fst <$> exprFields)
  then forM_ (zip (snd <$> definitionTys) (snd <$> exprFields)) (matchTyDefAgainstExpr env)
  else throwError RecordFieldExprNameMismatch

matchTyDefAgainstExpr :: Bullshit a => Environment a -> (Ty a, Expr a) -> TypeCheckM ()
matchTyDefAgainstExpr env (TigTOMBSTONE tn, e) = do
  et <- typeCheckE env e
  let tyDef = typeLookup env tn
  case (tyDef, et) of
    (Just TigRecord{}, TigNil) -> return ()
    (Just TigArray{}, TigNil) -> return ()
    (Just tyDef, et) -> unless (tyDef == et) $ throwError RecordExprTyFieldMismatch

matchTyDefAgainstExpr env (tyDef, e) = do
  eTy <- typeCheckE env e
  unless (tyDef == eTy) $ throwError RecordExprTyFieldMismatch

--TODO nil belongs to any record type
