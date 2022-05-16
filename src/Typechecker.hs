
module Typechecker (typecheck, showPos) where

import AbsMyLatte
import Data.Map

showPos :: BNFC'Position -> ShowS
showPos pos = case pos of
    Just (line, col) -> showString "(line:" . shows line . showString ", col:" . shows col . showString ")"
    Nothing -> id

--------------------- ŚRODOWISKA --------------------------------

type VType = Map Ident Type
type FType = Map Ident (Type, [Arg])

--------------------- PORÓWNYWANIE TYPÓW --------------------------------

eqTypes :: Type -> Type -> Bool
eqTypes t1 t2 = case (t1, t2) of
    (Int _, Int _) -> True
    (Str _, Str _) -> True
    (Bool _, Bool _) -> True
    (Void _, Void _) -> True
    (_, _) -> False

--------------------- TYPECKECK ARGUMENTÓW WYWOŁANIA FUNKCJI --------------------------------

checkArgs :: BNFC'Position -> (Type, [Arg]) -> [Expr] -> VType -> FType -> Either String Type
checkArgs pos (ft, args) exps gV gF =
    case (args, exps) of
        ([], []) -> Right ft
        (h : _, []) -> Left (showPos pos ": not enough arguments")
        ([], h : _) -> Left (showPos pos ": too many arguments")
        (arg : argRest, exp : expRest) ->
            case arg of
                ArgCp _ argt _ ->
                    case checkE exp gV gF of
                        Left error -> Left error
                        Right t ->
                            if eqTypes t argt then checkArgs pos (ft, argRest) expRest gV gF
                            else Left (showPos (hasPosition exp) ": wrong type of argument")
                ArgRef _ argt _ ->
                    case exp of
                        EVar _ id ->
                            if member id gV then
                                if eqTypes (gV ! id) argt then checkArgs pos (ft, argRest) expRest gV gF
                                else Left (showPos (hasPosition exp) ": wrong type of argument")
                            else Left (showPos (hasPosition exp) ": unknown identifier")
                        _ -> Left (showPos (hasPosition exp) ": expected identifier, got expression")

--------------------- TYPECHECK WYRAŻEŃ --------------------------------

checkE :: Expr -> VType -> FType -> Either String Type

checkE (EVar pos id) gV gF =
    if member id gV then Right (gV ! id)
    else Left (showPos pos ": undefined variable")

checkE (ELitInt pos val) gV gF = Right (Int pos)

checkE (ELitTrue pos) gV gF = Right (Bool pos)
checkE (ELitFalse pos) gV gF = Right (Bool pos)

checkE (EApp pos id exps) gV gF =
    if member id gF then
        checkArgs pos (gF ! id) exps gV gF
    else Left (showPos pos ": unknown function")

checkE (EString pos str) gV gF = Right (Str pos)

checkE (Not pos exp) gV gF = -- minus
    case checkE exp gV gF of
        Left error -> Left error
        Right (Bool _) -> Right (Bool pos)
        Right _ -> Left (showPos pos ": expected type bool")

checkE (Neg pos exp) gV gF = -- negacja binarna
    case checkE exp gV gF of
        Left error -> Left error
        Right (Int _) -> Right (Int pos)
        Right _ -> Left (showPos pos ": expected type int")

checkE (EMul pos exp1 op exp2) gV gF =
    case (checkE exp1 gV gF, checkE exp2 gV gF) of
        (Left error, _) -> Left error
        (Right (Int _), Left error) -> Left error
        (Right (Int _), Right (Int _)) -> Right (Int pos)
        (Right (Int _), Right _) -> Left (showPos (hasPosition exp2) ": expected type int")
        (Right _, _) -> Left (showPos (hasPosition exp1) ": expected type int")

checkE (EAdd pos exp1 op exp2) gV gF = checkE (EMul pos exp1 (Times (hasPosition op)) exp2) gV gF

-- typechecker przewiduje porównywanie wszystkich typów poza void
checkE (ERel pos exp1 op exp2) gV gF =
    case (checkE exp1 gV gF, checkE exp2 gV gF) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right t1, Right t2) ->
            if eqTypes t1 t2 then
                if eqTypes (Void Nothing) t1 then
                    Left (showPos (hasPosition exp1) ": operator undefined for type void")
                else Right (Bool pos)
            else Left (showPos (hasPosition op) ": incompatybile types")

checkE (EAnd pos exp1 exp2) gV gF =
    case (checkE exp1 gV gF, checkE exp2 gV gF) of
        (Left error, _) -> Left error
        (Right (Bool _), Left error) -> Left error
        (Right (Bool _), Right (Bool _)) -> Right (Bool pos)
        (Right (Bool _), Right _) -> Left (showPos (hasPosition exp2) ": expected type bool")
        (Right _, _) -> Left (showPos (hasPosition exp1) ": expected type bool")

checkE (EOr pos exp1 exp2) gV gF = checkE (EAnd pos exp1 exp2) gV gF

--------------------- TYPECHECK BLOKÓW --------------------------------
-- chcemy uniknąć sytacji
-- {
--     return 7;
--     return "xd";
-- }
checkB :: Block -> VType -> FType -> Maybe Type -> Either String (Maybe Type)

checkB block gV gF mT =
    case block of
    Block pos (head : tail) fdec stmt ->
        case checkDV head gV gF of -- ponowna deklaracja zmiennej o tym samym identyfikatorze przysłania poprzednią
            Left error -> Left error
            Right gV' -> checkB (Block pos tail fdec stmt) gV' gF mT
    Block pos [] (head : tail) stmt ->
        case checkDF head gV gF of
            Left error -> Left error
            Right gF' -> checkB (Block pos [] tail stmt) gV gF' mT
    Block pos [] [] (head : tail) ->
        case checkS head gV gF of
            Left error -> Left error
            Right mT' ->
                case (mT, mT') of
                    (Nothing, Nothing) -> checkB (Block pos [] [] tail) gV gF Nothing
                    (Nothing, Just t) -> checkB (Block pos [] [] tail) gV gF (Just t)
                    (Just t, Nothing) -> checkB (Block pos [] [] tail) gV gF (Just t)
                    (Just t1, Just t2) ->
                        if eqTypes t1 t2 then checkB (Block pos [] [] tail) gV gF (Just t1)
                        else Left ((showPos (hasPosition t1) . showPos (hasPosition t2)) ": ambigous return type")
    Block _ [] [] [] -> Right mT

--------------------- TYPECHECK INSTRUKCJI --------------------------------

checkS :: Stmt -> VType -> FType -> Either String (Maybe Type)

checkS (Empty _) gV gF = Right Nothing

checkS (BStmt _ block) gV gF = checkB block gV gF Nothing

checkS (Ass pos id exp1) gV gF =
    if member id gV then
        case (checkE exp1 gV gF, gV ! id) of
            (Left error, _) -> Left error
            (Right t1, t2) -> 
                if eqTypes t1 t2 then Right Nothing
                else Left (showPos pos ": incopatibile types")
    else Left (showPos pos ": undefined variable")

checkS (Incr pos id) gV gF =
    if member id gV then
        case gV ! id of
            (Int _) -> Right Nothing
            _ -> Left (showPos pos ": expected type int")
    else Left (showPos pos ": undefined variable")

checkS (Decr pos id) gV gF =
    if member id gV then
        case gV ! id of
            (Int _) -> Right Nothing
            _ -> Left (showPos pos ": expected type int")
    else Left (showPos pos ": undefined variable")

checkS (Ret _ exp) gV gF =
    case checkE exp gV gF of
        Left error -> Left error
        Right t -> Right (Just t)

checkS (VRet pos) gV gF = Right (Just (Void pos))

checkS (Cond _ exp block) gV gF =
    case checkE exp gV gF of
        Left error -> Left error
        Right (Bool _) -> checkS (BStmt (hasPosition block) block) gV gF
        Right _ -> Left (showPos (hasPosition exp) ": expected condition type bool")

checkS (CondElse pos exp block1 block2) gV gF =
    case checkE exp gV gF of
        Left error -> Left error
        Right (Bool _) ->
            case (checkS (BStmt (hasPosition block1) block1) gV gF, checkS (BStmt (hasPosition block2) block2) gV gF) of
                (Left error, _) -> Left error
                (_, Left error) -> Left error
                (Right Nothing, Right Nothing) -> Right Nothing
                (Right Nothing, Right (Just t)) -> Right (Just t)
                (Right (Just t), Right Nothing) -> Right (Just t)
                (Right (Just t1), Right (Just t2)) ->
                    if eqTypes t1 t2 then Right (Just t1)
                    else Left ((showPos (hasPosition t1) . showPos (hasPosition t2)) ": ambigous return type")
        Right _ -> Left (showPos (hasPosition exp) ": expected condition type bool")

checkS (While _ exp stmt) gV gF =
    case checkE exp gV gF of
        Left error -> Left error
        Right (Bool _) -> checkS stmt gV gF
        Right _ -> Left (showPos (hasPosition exp) ": expected condition type bool")

checkS (SExp _ exp) gV gF =
    case checkE exp gV gF of
        Left error -> Left error
        Right _ -> Right Nothing

--------------------- TYPECHECK DEKLARACJI ZMIENNYCH --------------------------------

checkDV :: VDecl -> VType -> FType -> Either String VType
checkDV decl gV gF =
    case decl of
        (VDecl _ _ []) -> Right gV
        (VDecl pos t (NoInit _ id : rest)) ->
            checkDV (VDecl pos t rest) (insert id t gV) gF
        (VDecl pos t (Init _ id exp : rest)) ->
            case checkE exp gV gF of
                Left error -> Left error
                Right t' ->
                    if eqTypes t t' then checkDV (VDecl pos t rest) (insert id t gV) gF
                    else Left (showPos (hasPosition exp) ": incompatybile types")

--------------------- TYPECHECK DEKLARACJI FUNKCJI --------------------------------

mapArgs :: [Arg] -> VType -> Either String VType
mapArgs args gV = case args of
    [] -> Right gV
    (ArgCp pos t id : rest) ->
        if member id gV then Left (showPos pos ": repeated argument name")
        else mapArgs rest (insert id t gV)
    (ArgRef pos t id : rest) ->
        if member id gV then Left (showPos pos ": repeated argument name")
        else mapArgs rest (insert id t gV)

checkDF :: FDecl -> VType -> FType -> Either String FType
checkDF (FDecl pos t id args block) gV gF =
    case mapArgs args empty of
        Left error -> Left error
        Right argMap ->
            case checkB block (argMap `union` gV) (insert id (t, args) gF) Nothing of
                Left error -> Left error
                Right Nothing -> Left (showPos pos ": missing return")
                Right (Just t') ->
                    if eqTypes t t' then Right (insert id (t, args) gF)
                    else Left (showPos (hasPosition t') ": wrong return type")

--------------------- TYPECHECK PROGRAMU --------------------------------

typecheck :: Program -> Maybe String
typecheck prog = case prog of
    Program _ [] -> Nothing
    Program pos (stmt : rest) -> case checkS stmt empty initFType of
        Left error -> Just ("Typecheck Failed! " ++ error)
        Right (Just (Void _)) -> typecheck (Program pos rest)
        Right (Just t) -> Just ("Typecheck Failed! " ++ showPos (hasPosition t) ": unexpected value returned")
        Right Nothing -> typecheck (Program pos rest)

initFType :: FType
initFType = fromList [
    (Ident "printInt", (Void Nothing, [ArgCp Nothing (Int Nothing) (Ident "x")])),
    (Ident "printStr", (Void Nothing, [ArgCp Nothing (Str Nothing) (Ident "x")])),
    (Ident "printBool", (Void Nothing, [ArgCp Nothing (Bool Nothing) (Ident "x")]))
    ]