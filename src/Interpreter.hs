module Interpreter (interpret) where

import AbsMyLatte
import Data.Map
import System.IO
import Typechecker (showPos)

data Value = IntVal Integer | StrVal String | BoolVal Bool| VoidVal deriving Show

type Loc = Integer
type VEnv = Map Ident Loc
type Store = Map Loc Value

type ArgVal = Either Value Loc
type FEnv = Map Ident (Type, [Arg], [ArgVal] -> Store -> IO (Either String (Maybe Value, Store)))

semRelOp :: Ord a => RelOp -> a -> a -> Bool
semRelOp op = case op of
  LTH _ -> (<)
  LE _ -> (<=)
  GTH _ -> (>)
  GE _ -> (>=)
  EQU _ -> (==)
  NE _ -> (/=)

--------------------- SEMANTYKA WYRAŹEŃ --------------------------------

semE :: Expr -> VEnv -> FEnv -> Store -> IO (Either String (Value, Store))

semE (EVar _ id) gV gF s = pure $ Right (s ! (gV ! id), s)

semE (ELitInt _ val) gV gF s = pure $ Right (IntVal val, s)

semE (ELitTrue _) gV gF s = pure $ Right (BoolVal True, s)
semE (ELitFalse _) gV gF s = pure $ Right (BoolVal False, s)

semE (EApp pos id exps) gV gF s =
    let (t, args, sem) = gF ! id in do
        res <- getArgs args exps gV gF s []
        case res of
            Left error -> pure $ Left error
            Right (argVals, s') -> do
                x <- sem argVals s'
                pure $ unpack pos x

semE (EString _ str) gV gF s = pure $ Right (StrVal str, s)

semE (Neg pos exp) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (IntVal val, s') -> pure $ Right (IntVal (-val), s')
        _ -> pure $ Left (showPos pos ": typecheck failure")

semE (Not pos exp) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (BoolVal val, s') -> pure $ Right (BoolVal (not val), s')
        _ -> pure $ Left (showPos pos ": typecheck failure")

semE (EMul _ exp1 op exp2) gV gF s = do
    res <- opArgs exp1 exp2 gV gF s
    case res of
        Left error -> pure $ Left error
        Right (IntVal val1, IntVal val2, s') ->
            case (op, val2) of
                (Times _, _) -> pure $ Right (IntVal (val1 * val2), s')
                (Div pos, 0) -> pure $ Left (showPos pos ": division by 0")
                (Div _, _) -> pure $ Right (IntVal (div val1 val2), s')
                (Mod pos, 0) -> pure $ Left (showPos pos ": division by 0")
                (Mod _, _) -> pure $ Right (IntVal (mod val1 val2), s')
        _ -> pure $ Left (showPos (hasPosition op) ": typecheck failure")

semE (EAdd _ exp1 op exp2) gV gF s = do
    res <- opArgs exp1 exp2 gV gF s
    case res of
        Left error -> pure $ Left error
        Right (IntVal val1, IntVal val2, s') ->
            case op of
                Plus _ -> pure $ Right (IntVal (val1 + val2), s')
                Minus _ -> pure $ Right (IntVal (val1 - val2), s')
        _ -> pure $ Left (showPos (hasPosition op) ": typecheck failure")

semE (ERel _ exp1 op exp2) gV gF s = do
    res <- opArgs exp1 exp2 gV gF s
    case res of
        Left error -> pure $ Left error
        Right (IntVal val1, IntVal val2, s') -> pure $ Right (BoolVal (semRelOp op val1 val2), s')
        Right (StrVal val1, StrVal val2, s') -> pure $ Right (BoolVal (semRelOp op val1 val2), s')
        Right (BoolVal val1, BoolVal val2, s') -> pure $ Right (BoolVal (semRelOp op val1 val2), s')
        _ -> pure $ Left (showPos (hasPosition op) ": typecheck failure")

semE (EAnd pos exp1 exp2) gV gF s = do
    res1 <- semE exp1 gV gF s
    case res1 of
        Left error -> pure $ Left error
        Right (BoolVal True, s1) -> do
            res2 <- semE exp2 gV gF s1
            case res2 of
                Left error -> pure $ Left error
                Right (val2, s2) -> pure $ Right (val2, s2)
        Right (BoolVal False, s1) -> pure $ Right (BoolVal False, s1)
        _ -> pure $ Left (showPos pos ": typecheck failure")

semE (EOr pos exp1 exp2) gV gF s = do
    res1 <- semE exp1 gV gF s
    case res1 of
        Left error -> pure $ Left error
        Right (BoolVal False, s1) -> do
            res2 <- semE exp2 gV gF s1
            case res2 of
                Left error -> pure $ Left error
                Right (val2, s2) -> pure $ Right (val2, s2)
        Right (BoolVal True, s1) -> pure $ Right (BoolVal True, s1)
        _ -> pure $ Left (showPos pos ": typecheck failure")

--------------------- FUNKCJA POMOCNICZA WYLICZAJĄCA WARTOŚCI ARGUMENTÓW --------------------------------

opArgs :: Expr -> Expr -> VEnv -> FEnv -> Store -> IO (Either String (Value, Value, Store))
opArgs exp1 exp2 gV gF s = do
    res1 <- semE exp1 gV gF s
    case res1 of
        Left error -> pure $ Left error
        Right (val1, s1) -> do
            res2 <- semE exp2 gV gF s1
            case res2 of
                Left error -> pure $ Left error
                Right (val2, s2) -> pure $ Right (val1, val2, s2)

--------------------- SEMANTYKA INSTRUKCJI --------------------------------

semS :: Stmt -> VEnv -> FEnv -> Store -> IO (Either String (Maybe Value, Store))

semS (Empty _) gV gF s = pure $ Right (Nothing, s)

semS (BStmt pos block) gV gF s =
    case block of
        Block posb (head : tail) fdec stmt -> do
            resDV <- semDV head gV gF s
            case resDV of
                Left error -> pure $ Left error
                Right (gV', s') -> semS (BStmt pos (Block posb tail fdec stmt)) gV' gF s'
        Block posb [] (head : tail) stmt ->
            semS (BStmt pos (Block posb [] tail stmt)) gV (semDF head gV gF) s
        Block posb [] [] (head : tail) -> do
            res <- semS head gV gF s
            case res of
                Left error -> pure $ Left error
                Right (Nothing, s') -> semS (BStmt pos (Block posb [] [] tail)) gV gF s'
                Right (Just val, s') -> pure $ Right (Just val, s') -- return w bloku powoduje niewykonanie dalszych instrukcji
        Block _ [] [] [] -> pure $ Right (Nothing, s)

semS (Ass _ id exp) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (val, s') -> pure $ Right (Nothing, insert (gV ! id) val s')

semS (Incr pos id) gV gF s =
    case s ! (gV ! id) of
        IntVal val -> pure $ Right (Nothing, insert (gV ! id) (IntVal (val + 1)) s)
        _ -> pure $ Left (showPos pos ": typecheck failure")

semS (Decr pos id) gV gF s =
    case s ! (gV ! id) of
        IntVal val -> pure $ Right (Nothing, insert (gV ! id) (IntVal (val - 1)) s)
        _ -> pure $ Left (showPos pos ": typecheck failure")

semS (Ret _ exp) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (val, s') -> pure $ Right (Just val, s')

semS (VRet _) gV gF s = pure $ Right (Just VoidVal, s)

semS (Cond pos exp block) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (BoolVal True, s') -> semS (BStmt (hasPosition block) block) gV gF s'
        Right (BoolVal False, s') -> pure $ Right (Nothing, s')
        _ -> pure $ Left (showPos pos ": typecheck failure")

semS (CondElse pos exp block1 block2) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (BoolVal True, s') -> semS (BStmt (hasPosition block1) block1) gV gF s'
        Right (BoolVal False, s') -> semS (BStmt (hasPosition block2) block2) gV gF s'
        _ -> pure $ Left (showPos pos ": typecheck failure")

semS (While pos exp stmt) gV gF s = do
    resE <- semE exp gV gF s
    case resE of
        Left error -> pure $ Left error
        Right (BoolVal False, s') -> pure $ Right (Nothing, s')
        Right (BoolVal True, s1) -> do
            resS <- semS stmt gV gF s1
            case resS of
                Left error -> pure $ Left error
                Right (Nothing, s2) -> semS (While pos exp stmt) gV gF s2
                Right (Just val, s2) -> pure $ Right (Just val, s2) -- return w ciele while powoduje zaprzestanie wykonania
        _ -> pure $ Left (showPos pos ": typecheck failure")

semS (SExp _ exp) gV gF s = do
    res <- semE exp gV gF s
    case res of
        Left error -> pure $ Left error
        Right (_, s') -> pure $ Right (Nothing, s')

--------------------- SEMANTYKA DEKLARACJI ZMIENNYCH --------------------------------

semDV :: VDecl -> VEnv -> FEnv -> Store -> IO (Either String (VEnv, Store))
semDV decl gV gF s =
    let newloc = toInteger (size s) in
        case decl of
            (VDecl _ _ []) -> pure $ Right (gV, s)
            (VDecl pos (Int post) ((NoInit _ id) : rest)) ->
                semDV (VDecl pos (Int post) rest) (insert id newloc gV) gF (insert newloc (IntVal (toInteger 0)) s)
            (VDecl pos (Str post) ((NoInit _ id) : rest)) ->
                semDV (VDecl pos (Str post) rest) (insert id newloc gV) gF (insert newloc (StrVal "") s)
            (VDecl pos (Bool post) ((NoInit _ id) : rest)) ->
                semDV (VDecl pos (Bool post) rest) (insert id newloc gV) gF (insert newloc (BoolVal False) s)
            (VDecl pos (Void post) ((NoInit _ id) : rest)) ->
                semDV (VDecl pos (Void post) rest) (insert id newloc gV) gF (insert newloc VoidVal s)
            (VDecl pos t ((Init _ id exp) : rest)) -> do
                res <- semE exp gV gF s
                case res of
                    Left error -> pure $ Left error
                    Right (val, s') -> semDV (VDecl pos t rest) (insert id newloc gV) gF (insert newloc val s')

--------------------- FUNKCJIE POMOCNICZE DO APLIKACJI FUNKCJI --------------------------------

getArgs :: [Arg] -> [Expr] -> VEnv -> FEnv -> Store -> [ArgVal] -> IO (Either String ([ArgVal], Store))
getArgs args exps gV gF s acc =
    case (args, exps) of
        (ArgCp {} : argRest, exp : expRest) -> do
            res <- semE exp gV gF s
            case res of
                Left error -> pure $ Left error
                Right (val, s') -> getArgs argRest expRest gV gF s' (Left val : acc)
        (ArgRef {} : argRest, EVar _ id : expRest) -> getArgs argRest expRest gV gF s (Right (gV ! id) : acc)
        ([], []) -> pure $ Right (reverse acc, s)
        _ -> pure $ Left "typecheck failure"

unpack :: BNFC'Position -> Either String (Maybe Value, Store) -> Either String (Value, Store)
unpack pos packed = case packed of
    Left error -> Left error
    Right (Nothing, _) -> Left (showPos pos ": function hasn't returned")
    Right (Just val, s) -> Right (val, s)

addArgs :: [Arg] -> [ArgVal] -> VEnv -> Store -> Either String (VEnv, Store)
-- żaden z błędów nie powinien wystąpić, jeśli checkArgs działa poprawnie
addArgs args argVals gV s =
    let newloc = toInteger (size s) in
        case (args, argVals) of
            (ArgCp _ _ id : argRest, Left val : argValRest) ->
                addArgs argRest argValRest (insert id newloc gV) (insert newloc val s)
            (ArgRef _ _ id : argRest, Right loc : argValRest) ->
                addArgs argRest argValRest (insert id loc gV) s
            ([], []) -> Right (gV, s)
            _ -> Left "typecheck failure"

--------------------- SEMANTYKA FUNKCJI --------------------------------

semF :: Block -> VEnv -> FEnv -> Ident -> Type -> [Arg] -> [ArgVal] -> Store -> IO (Either String (Maybe Value, Store))
semF block gV gF id t args argVals s =
    case addArgs args argVals gV s of
        Left error -> pure $ Left error
        Right (gV', s') ->
            semS (BStmt (hasPosition block) block) gV' (insert id (t, args, semF block gV gF id t args) gF) s'

--------------------- SEMANTYKA DEKLARACJI FUNKCJI --------------------------------

semDF :: FDecl -> VEnv -> FEnv -> FEnv
semDF (FDecl _ t id args block) gV gF =
    let sem = semF block gV gF id t args in
        insert id (t, args, sem) gF

--------------------- PREDEFINIOWANE FUNKCJE DO WYPISYWANIA --------------------------------

printInt :: [ArgVal] -> Store -> IO (Either String (Maybe Value, Store))
printInt argVals s =
    case argVals of
        [Left (IntVal val)] -> do
            print val
            pure $ Right (Just VoidVal, s)
        _ -> pure $ Left "argCheck failed"

printStr :: [ArgVal] -> Store -> IO (Either String (Maybe Value, Store))
printStr argVals s =
    case argVals of
        [Left (StrVal val)] -> do
            print val
            pure $ Right (Just VoidVal, s)
        _ -> pure $ Left "argCheck failed"

printBool argVals s =
    case argVals of
        [Left (BoolVal val)] -> do
            print val
            pure $ Right (Just VoidVal, s)
        _ -> pure $ Left "argCheck failed"

initFEnv = fromList [
    (Ident "printInt", (Void Nothing, [ArgCp Nothing (Int Nothing) (Ident "x")],printInt)),
    (Ident "printStr", (Void Nothing, [ArgCp Nothing (Str Nothing) (Ident "x")], printStr)),
    (Ident "printBool", (Void Nothing, [ArgCp Nothing (Bool Nothing) (Ident "x")], printBool))
    ]

interpret :: Program -> IO ()
interpret (Program pos list)  =
    case list of
        [] -> pure ()
        (stmt : rest) -> do
            res <- semS stmt empty initFEnv empty
            case res of
                Left error -> hPutStrLn stderr ("Error! " ++ error)
                Right (Just _, s') -> pure()
                Right (Nothing, s') -> interpret (Program pos rest)
