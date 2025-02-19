module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env =
    case H.lookup s env of
        Just v -> v
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env =
    case H.lookup op intOps of
        Just fop ->
            case eval e2 env of
                IntVal 0 | op == "/" ->  ExnVal "Division by 0"
                y -> liftIntOp fop (eval e1 env) y
        Nothing -> ExnVal "Unknown op"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    case H.lookup op boolOps of
        Just bop -> liftBoolOp bop (eval e1 env) (eval e2 env)
        Nothing -> ExnVal "Unknown op"

eval (CompOpExp op e1 e2) env =
    case H.lookup op compOps of
        Just cop -> liftCompOp cop (eval e1 env) (eval e2 env)
        Nothing -> ExnVal "Unknown op"

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    case (eval e1 env) of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application
-- create closure
eval (FunExp params body) env = CloVal params body env

-- e1 is CloVal,  params body env
eval (AppExp e1 args) env =
    case eval e1 env of
        CloVal params body clenv ->
            let combenv = H.union (H.fromList (zip params (map (\arg -> eval arg env) args))) clenv
                in eval body combenv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env =
    let newenv = H.union (H.fromList (map (\(x,y) -> (x, eval y env) ) pairs)) env
        in eval body newenv

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, newenv)
    where newenv = H.insert var (eval e env) env

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env =
    let (val, temp_penv, temp_env) = exec x penv env
        (rest_val, final_penv, final_env) = exec (SeqStmt xs) temp_penv temp_env
    in (val ++ rest_val, final_penv, final_env)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
    case eval e1 env of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _-> (s, penv, env)
            where s = show (ExnVal "Condition is not a Bool")

--- ### Procedure and Call Statements

-- insert a procedure into penv
exec p@(ProcedureStmt name args body) penv env = ("", newpenv, env)
    where newpenv = H.insert name p penv

exec (CallStmt name args) penv env =
    case H.lookup name penv of
        Just (ProcedureStmt _ params body) -> let temp_env = H.union (H.fromList $ zip params (map (`eval` env) args )) env
            in exec body penv temp_env
        Nothing->("Procedure "++ name ++ " undefined", penv, env)