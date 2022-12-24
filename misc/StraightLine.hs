import Data.Foldable (foldl')
import Control.Monad (void)

type Identifier = String

data Statement = CompoundStm Statement Statement -- Stm ; Stm
               | AssignmentStm Identifier Expr   -- Id := Expr
               | PrintStm [Expr]                 --
               deriving Show

data Expr = IdExpr Identifier           -- Var
          | NumExpr Float               -- Float literal
          | OpExpr Expr BinOp Expr      -- E1 Bop E2
          | ESeqExpr Statement Expr     -- (Stm, Expr)
           deriving Show

data BinOp = Plus
           | Minus
           | Mul
           | Div
           deriving Show

-- Pg 11.2
interp :: Statement -> IO ()
interp s = void $ interp' s []

interp' :: Statement -> [(Identifier, Expr)] -> IO [(Identifier, Expr)]
interp' (CompoundStm s xs) env = interp' s env >>= interp' xs

interp' (AssignmentStm i e) env = do
  (e', env') <- eval e env
  return $ updateAL i e' env'

interp' (PrintStm []) env = return env

interp' (PrintStm (e:es)) env = do
  (e', env') <- eval e env
  print e'
  interp' (PrintStm es) env'

eval :: Expr -> [(Identifier, Expr)] -> IO (Expr, [(Identifier, Expr)])
eval (IdExpr i) env = return (lookupAL i env, env)
eval e@(NumExpr _) env = return (e, env)
eval (ESeqExpr s e) env = interp' s env >>= eval e

eval (OpExpr e1 bop e2) env = do
  (e1', env') <- eval e1 env
  (e2', env'') <- eval e2 env'
  return (evalBop bop e1' e2', env'')

updateAL :: Identifier -> Expr -> [(Identifier, Expr)] -> [(Identifier, Expr)]
updateAL i e [] = [(i,e)]
updateAL i e (x@(v,_) : xs) | i == v = (i, e) : xs
                            | otherwise = x : updateAL i e xs

lookupAL :: Identifier -> [(Identifier, Expr)] -> Expr
lookupAL i [] = error $ "Undeclared indentifier access: " <> i
lookupAL i ((v,e) : xs) | i == v = e
                        | otherwise = lookupAL i xs

evalBop :: BinOp -> Expr -> Expr -> Expr
evalBop Plus (NumExpr x) (NumExpr y) = NumExpr $ x + y
evalBop Minus (NumExpr x) (NumExpr y) = NumExpr $ x - y
evalBop Mul (NumExpr x) (NumExpr y) = NumExpr $ x * y
evalBop Div (NumExpr x) (NumExpr y) = NumExpr $ x / y
evalBop op e1 e2 = error $ "Unevaluated operands " <> show op <> " " <> show e1 <> " " <> show e2


tprog :: Statement
tprog = CompoundStm
          (AssignmentStm "a" (OpExpr (NumExpr 5) Plus (NumExpr 3)))
        (CompoundStm
          (AssignmentStm "b"
            (ESeqExpr (PrintStm [ IdExpr "a" , OpExpr (IdExpr "a") Minus (NumExpr 1) ])
                      (OpExpr (NumExpr 10) Mul (IdExpr "a"))))
        (PrintStm [ IdExpr "b" ]))

{-
 - a := 5 + 3;
 - b := ( print ( a , a - 1 ) , 10 * a;
 - print ( b )
 - 8
 - 7
 - 80
-}

-- x := 1;
-- y := (x := x + 1, 5); ESeqExpr should eval to 5 and assign y as 5, not (x:= x + 1, 5).
-- print (x); -- 2
-- print (y); -- 5
-- print (x); -- 2
-- print (y); -- 5

tprogStrict :: Statement
tprogStrict =
   CompoundStm
      (AssignmentStm "x" (NumExpr 1))
   (CompoundStm
      (AssignmentStm "y"
        (ESeqExpr
          (AssignmentStm "x" (OpExpr (IdExpr "x") Plus (NumExpr 1)))
          (NumExpr 5)))
   (CompoundStm
      (PrintStm [IdExpr "x"])
   (CompoundStm
      (PrintStm [IdExpr "y"])
   (CompoundStm
      (PrintStm [IdExpr "x"])
   (PrintStm [IdExpr "y"])))))

-- print ((print 1, 2))
-- 1
-- 2
tprogNestedprint :: Statement
tprogNestedprint
  = PrintStm [ESeqExpr (PrintStm [NumExpr 1]) (NumExpr 2)]

-- x:= 1;
-- print ( ((x := 2), 5)   -- Print order
--        ,((x := 3), 10))                 -- 5
--                                         -- 10
-- print x;                                -- 3
--
-- print ((x := 4, 420) + ((x := 5), 420)) -- 840 OpExpr order
-- print x;                                -- 5

tprogEvaluationOrder :: Statement
tprogEvaluationOrder =
  CompoundStm
    (AssignmentStm "x" (NumExpr 1))
 (CompoundStm
    (PrintStm  [ ESeqExpr (AssignmentStm "x" (NumExpr 2)) (NumExpr 5)
                ,ESeqExpr (AssignmentStm "x" (NumExpr 3)) (NumExpr 10)])
 (CompoundStm
    (PrintStm [IdExpr "x"])
 (CompoundStm
    (PrintStm [ OpExpr (ESeqExpr (AssignmentStm "x" (NumExpr 4)) (NumExpr 420))
                       Plus
                       (ESeqExpr (AssignmentStm "x" (NumExpr 5)) (NumExpr 420))])
 (PrintStm [IdExpr "x"]))))

-- x := 1
-- x := 2
-- print x -- 2
tprogCompoundEvalOrder =
  CompoundStm
    (AssignmentStm "x" (NumExpr 1))
 (CompoundStm
    (AssignmentStm "x" (NumExpr 2))
 (PrintStm [IdExpr "x"]))

-- Pg 11.1
maxArgs :: Statement -> Int
maxArgs (CompoundStm s1 s2) = max (maxArgs s1) (maxArgs s2)
maxArgs (AssignmentStm _ e) = maxEArgs e
maxArgs (PrintStm xs) = max (length xs) childMaxPrintArgs
  where childMaxPrintArgs = foldl' max 0 $ fmap maxEArgs xs

maxEArgs :: Expr -> Int
maxEArgs (IdExpr _)       = 0
maxEArgs (NumExpr _)      = 0
maxEArgs (OpExpr e1 _ e2) = max (maxEArgs e1) (maxEArgs e2)
maxEArgs (ESeqExpr s e)   = max (maxArgs s) (maxEArgs e)
