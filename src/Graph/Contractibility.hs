module Graph.Contractibility
  ( validateContractibility
  )
where

import Control.Monad (foldM)
import qualified Data.Set as S
import Graph.Types
import Graph.UnionFind

-- Helper to collect all vars
getAllNodes :: [Link] -> S.Set Node
getAllNodes links = S.fromList $ concatMap getNodes links
 where
  getNodes (Strong ns) = ns
  getNodes (Weak t ns) = t : ns

-- Main Validation Function
validateContractibility :: [Link] -> Either String ()
validateContractibility links = do
  let
    allNodes = getAllNodes links
  -- Initialize UF
  let
    uf0 = emptyUF

  -- Phase 1: Process Strong Links
  ufStrong <- foldM processStrong uf0 links

  -- Phase 2: Process Weak Links (Worklist)
  let
    weakLinks = [l | l@(Weak _ _) <- links]
  ufFinal <- processWeakLoop weakLinks ufStrong

  -- Phase 3: Final Checks
  -- 1. Check if any Weak links remain "pending" (disjoint inputs)
  checkRemainingWeak weakLinks ufFinal

  -- 2. Check Global Connectivity (Single Root)
  checkConnectedness (S.toList allNodes) ufFinal
 where
  processStrong :: UnionFind -> Link -> Either String UnionFind
  processStrong uf (Weak _ _) = Right uf -- toggle weak links later
  processStrong uf (Strong nodes) =
    -- Merge all nodes in the list.
    -- "If items already same node -> Cycle Error"
    mergeList nodes uf
   where
    mergeList [] u = Right u
    mergeList [_] u = Right u
    mergeList (x : y : xs) u = do
      let
        (rootX, u1) = find x u
      let
        (rootY, u2) = find y u1
      if rootX == rootY
        then Left $ "Cycle Error: Hard loop detected at Strong link involving " ++ show x ++ " and " ++ show y
        else mergeList (y : xs) (union x y u2)

  processWeakLoop :: [Link] -> UnionFind -> Either String UnionFind
  processWeakLoop weaks uf = loop weaks uf True
   where
    loop [] currentUF _ = Right currentUF
    loop pending currentUF changed
      | not changed = Right currentUF -- No more progress possible
      | otherwise = do
          (newPending, newUF, hasChanged) <- foldM step ([], currentUF, False) pending
          loop newPending newUF hasChanged

    step (pends, u, chg) link@(Weak target sources) = do
      -- Check if all sources are in the same group
      let
        (u1, allSame) = checkSourcesSameGroup sources u
      if allSame
        then do
          -- Merge target to that group
          let
            inputsHead = head sources -- safe if size >= 1
          let
            u2 = union target inputsHead u1
          return (pends, u2, True)
        else
          -- Pending
          return (link : pends, u1, chg)
    step acc _ = Right acc -- Should not happen for weak list
  checkSourcesSameGroup :: [Node] -> UnionFind -> (UnionFind, Bool)
  checkSourcesSameGroup [] u = (u, True) -- empty sources? true
  checkSourcesSameGroup (x : xs) u =
    let
      (rootX, u1) = find x u
      f (currU, same) y =
        let
          (rootY, nextU) = find y currU
        in
          (nextU, same && (rootX == rootY))
    in
      foldl f (u1, True) xs

  checkRemainingWeak :: [Link] -> UnionFind -> Either String ()
  checkRemainingWeak weaks uf =
    foldM_ check (uf, ()) weaks
   where
    check (u, _) (Weak _ sources) =
      let
        (u1, same) = checkSourcesSameGroup sources u
      in
        if same
          then Right (u1, ())
          else Left "Connectivity Error: Par inputs represent disjoint structures (Need Mix rule)."
    check acc _ = Right acc

  checkConnectedness :: [Node] -> UnionFind -> Either String ()
  checkConnectedness [] _ = Right ()
  checkConnectedness (n : ns) uf =
    let
      (root, u1) = find n uf
      checkNode (u, _) x =
        let
          (r, u2) = find x u
        in
          if r /= root
            then Left "Graph is disconnected (Need Mix rule)."
            else Right (u2, ())
    in
      foldM_ checkNode (u1, ()) ns

  foldM_ f a l = foldM f a l >> return ()
