{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.TypeCheck
  ( checkProgramIO
  , Env
  , GlobalEnv
  )
where

import Control.Monad (unless, zipWithM, zipWithM_)
import Control.Monad.Except
import Control.Monad.State
import Data.List (unsnoc)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Graph.Contractibility as G
import qualified Graph.Types as G
import Language.Syntax

-- liftIO imported via Control.Monad.State -> Control.Monad.IO.Class

-- Environment: Name -> Type
type Env = Map Name Type

type GlobalEnv = Map Name Type

-- Type Checking State
type NodeMap = Map Name G.Node

type CheckM a = StateT CheckState (ExceptT String IO) a -- Switched to IO for printing check logs if needed, or simple ID.
-- Keeping it simple: pure CheckM handled by runCheck ignoring IO unless we need debug.
-- Using (Except String) implies pure.
-- But `liftIO` usage in `checkProgram` requires IO.
-- So stack is `StateT CheckState (ExceptT String IO)`.

data CheckState = CheckState
  { availableVars :: Map Name (Type, G.Node)
  , nodeMap :: NodeMap
  , links :: [G.Link]
  , freshCount :: Int
  , globalDef :: GlobalEnv
  , typeAliases :: Map Name Type
  }

initialState :: CheckState
initialState = CheckState M.empty M.empty [] 0 M.empty M.empty

runCheck :: CheckM a -> IO (Either String (a, CheckState))
runCheck m = runExceptT (runStateT m initialState)

-- Helpers
fresh :: CheckM String
fresh = do
  s <- get
  let
    n = freshCount s
  put s{freshCount = n + 1}
  return $ "$_" ++ show n

registerVar :: Name -> CheckM G.Node
registerVar x = do
  s <- get
  case M.lookup x (nodeMap s) of
    Just n -> return n
    Nothing -> do
      let
        n = x
      put s{nodeMap = M.insert x n (nodeMap s)}
      return n

registerFreshNode :: CheckM G.Node
registerFreshNode = fresh

addLink :: G.Link -> CheckM ()
addLink l = do
  s <- get
  put s{links = l : links s}

consume :: Name -> CheckM (Type, G.Node)
consume x = do
  s <- get
  case M.lookup x (availableVars s) of
    Just (t, n) -> do
      put s{availableVars = M.delete x (availableVars s)}
      return (t, n)
    Nothing -> throwError $ "Linearity Error: Variable '" ++ x ++ "' used more than once or not available."

introduce :: Name -> Type -> CheckM G.Node
introduce x t = do
  s <- get
  n <- registerVar x
  case M.lookup x (availableVars s) of
    Just _ -> throwError $ "Shadowing Error: Variable '" ++ x ++ "' already exists (linear shadowing not allowed)."
    Nothing -> put s{availableVars = M.insert x (t, n) (availableVars s)}
  return n

linkStrong :: [G.Node] -> CheckM ()
linkStrong ns = addLink (G.Strong ns)

linkWeak :: G.Node -> [G.Node] -> CheckM ()
linkWeak t srcs = addLink (G.Weak t srcs)

-- Flatten Par
-- Compute Dual
desugarType :: Type -> Type
desugarType (TFun args ret) = TPar (map (desugarType . TDual) args ++ [desugarType ret])
desugarType (TDual t) = case desugarType t of
  TAtom n -> TDual (TAtom n)
  TDual t' -> t'
  TTensor ts -> TPar (map (desugarType . TDual) ts) -- Dual of Tensor is Par
  TPar ts -> TTensor (map (desugarType . TDual) ts) -- Dual of Par is Tensor
  -- ~((A, B) => C) = ~{~A, ~B, C} = [A, B, ~C]
  TFun args ret -> TTensor (map desugarType args ++ [desugarType $ TDual ret])
desugarType (TTensor ts) = TTensor (map desugarType ts)
desugarType (TPar ts) = TPar (map desugarType ts)
desugarType t = t

-- | 型が意味として同じか
eqTy :: Type -> Type -> Bool
eqTy t1 t2 = desugarType t1 == desugarType t2

expandType :: Type -> CheckM Type
expandType t = case t of
  TAtom n -> do
    s <- get
    case M.lookup n (typeAliases s) of
      Just alias -> expandType alias
      Nothing -> return t
  TDual t' -> TDual <$> expandType t'
  TTensor ts -> TTensor <$> mapM expandType ts
  TPar ts -> TPar <$> mapM expandType ts
  TFun args ret -> do
    eArgs <- mapM expandType args
    eRet <- expandType ret
    return (TFun eArgs eRet)

checkProgramIO :: Program -> IO ()
checkProgramIO (Program tops) = do
  res <- runCheck (mapM_ checkTopLevel tops)
  case res of
    Left err -> fail err
    Right _ -> return ()

checkTopLevel :: TopLevel -> CheckM ()
checkTopLevel (TypeDef n t) = do
  s <- get
  put s{typeAliases = M.insert n t (typeAliases s)}
checkTopLevel (GlobalLet n mt term) = do
  s <- get
  put s{availableVars = M.empty, links = [], freshCount = 0}

  (inferedT, _) <- checkTerm term
  displayT <- case mt of
    Just expectedT -> do
      expandedExpectedT <- expandType expectedT
      unless (eqTy inferedT expandedExpectedT) $ throwError $ "Type Mismatch: Expected " ++ printType expandedExpectedT ++ " but got " ++ printType inferedT
      return expandedExpectedT
    Nothing -> return inferedT

  -- Linearity Check: All vars consumed?
  finalS <- get
  unless (M.null $ availableVars finalS) $
    throwError $
      "Linearity Error: Unused variables: " ++ show (M.toList $ availableVars finalS)

  finalLinks <- gets links
  case G.validateContractibility finalLinks of
    Left err -> throwError $ "DR Error in " ++ n ++ ": " ++ err
    Right () -> liftIO $ putStrLn $ "Type Check Passed: " ++ n ++ " : " ++ printType displayT

-- checkTerm stub: only used for inference context if needed.
checkTerm :: Term -> CheckM (Type, G.Node)
checkTerm tm = case tm of
  Block stmts ret -> do
    preVars <- gets availableVars
    mapM_ checkStmt stmts
    (retTy, node) <- checkTerm ret
    postVars <- gets availableVars
    let
      unconsumedLocals = M.difference postVars preVars
    unless (M.null unconsumedLocals) $
      throwError $
        "Linearity Error: Local variables defined in block not consumed: " ++ show (M.toList unconsumedLocals)
    return (retTy, node)
  Var x -> do
    (ty, node) <- consume x
    return (ty, node)
  Tensor tms -> do
    tns <- mapM checkTerm tms
    node <- registerFreshNode
    linkStrong $ node : map snd tns
    return (TTensor $ map fst tns, node)
  Par tms -> do
    tns <- mapM checkTerm tms
    node <- registerFreshNode
    linkWeak node $ map snd tns
    return (TPar $ map fst tns, node)
  Lambda params body -> do
    preVars <- gets availableVars
    -- introduce params
    xtns :: [(Name, Type, G.Node)] <-
      mapM
        ( \(x, t) -> do
            expandedT <- expandType t
            n <- introduce x expandedT
            return (x, expandedT, n)
        )
        params
    -- check body
    (bodyTy, bodyNode) <- checkTerm body
    postVars <- gets availableVars
    let
      unconsumedLocals = M.difference postVars preVars
    unless (M.null unconsumedLocals) $
      throwError $
        "Linearity Error: Local variables defined in lambda not consumed: " ++ show (M.toList unconsumedLocals)
    node <- registerFreshNode
    -- Weak link: Args & Ret -> Node
    linkWeak node $ map (\(_, _, n) -> n) xtns ++ [bodyNode]
    return (TFun (map snd params) bodyTy, node)
  App f args -> do
    (fTy, fNode) <- checkTerm f
    -- fTy を desuger すると Par [...~args, ret] でなければいけない。
    let
      desugaredFTy = desugarType fTy
    case desugaredFTy of
      TPar par | Just (dualArgTs, retT) <- unsnoc par -> do
        argTns <- mapM checkTerm args
        zipWithM_
          (\expectedDualArgT actualArgT -> unless (eqTy (TDual expectedDualArgT) actualArgT) $ throwError $ "Type Mismatch: Expected " ++ printType (desugarType $ TDual expectedDualArgT) ++ " but got " ++ printType actualArgT)
          dualArgTs
          (map fst argTns)
        -- Strong Link
        linkStrong $ fNode : map snd argTns
        return (retT, fNode)
      _ -> throwError "App Error: Not a function"

checkStmt :: Stmt -> CheckM ()
checkStmt stmt = case stmt of
  Let x tm -> do
    (inferedT, node) <- checkTerm tm
    xNode <- introduce x inferedT
    linkStrong [xNode, node]
  LetTensor xs tm -> do
    (inferedT, node) <- checkTerm tm
    let
      -- Desugered inferedT should be Tensor [t,...]
      desugaredInferedT = desugarType inferedT
    case desugaredInferedT of
      TTensor ts -> do
        unless (length xs == length ts) $ throwError "LetTensor Error: Number of variables does not match"
        -- Weak link
        nodes <- zipWithM introduce xs ts
        linkWeak node nodes
      _ -> throwError "LetTensor Error: Not a tensor"
  LetPar xs tm -> do
    (inferedT, node) <- checkTerm tm
    let
      -- Desugered inferedT should be Par [t,...]
      desugaredInferedT = desugarType inferedT
    case desugaredInferedT of
      TPar ts -> do
        unless (length xs == length ts) $ throwError "LetPar Error: Number of variables does not match"
        -- Strong Link
        nodes <- zipWithM introduce xs ts
        linkStrong $ node : nodes
      _ -> throwError "LetPar Error: Not a par"
  Intro x y t -> do
    expandedT <- expandType t
    xNode <- introduce x $ TDual expandedT
    yNode <- introduce y expandedT
    linkStrong [xNode, yNode]
  Elim t1 t2 -> do
    (inferedT1, node1) <- checkTerm t1
    (inferedT2, node2) <- checkTerm t2
    let
      -- dual of t1 == t2
      desugeredDualT1 = desugarType $ TDual inferedT1
      desugeredT2 = desugarType inferedT2
    unless (eqTy desugeredDualT1 desugeredT2) $ throwError "Elim Error: Duals do not match"
    linkStrong [node1, node2]