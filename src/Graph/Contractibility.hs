{-# LANGUAGE ScopedTypeVariables #-}

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

  -- 2. Check Global Connectivity (Single Root)
  checkConnectedness (S.toList allNodes) ufFinal
 where
  processStrong :: UnionFind Node -> Link -> Either String (UnionFind Node)
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
        (_, rootX, u1) = find x u
        (_, rootY, u2) = find y u1
      if rootX == rootY
        then Left $ "Cycle Error: Hard loop detected at Strong link involving " ++ show x ++ " and " ++ show y
        else mergeList (y : xs) (union x y u2)

  processWeakLoop :: [Link] -> UnionFind Node -> Either String (UnionFind Node)
  processWeakLoop weaks uf = loop weaks uf True
   where
    loop [] currentUF _ = Right currentUF
    loop pending currentUF changed
      | not changed && null pending = Right currentUF -- No more progress possible
      | not changed = Left "Connectivity Error: Par inputs represent disjoint structures (Maybe need Mix rule)."
      | otherwise = do
          (newPending, newUF, hasChanged) <- foldM step ([], currentUF, False) pending
          loop newPending newUF hasChanged

    step (pends, u, _) (Weak target []) = do
      -- Source is empty, that is, botoom.
      -- bottom nodes can be merged to any node.
      -- but we can't know the most preferred node just now
      -- add metadata to union find
      let
        u1 = addMeta target 1 u
      return (pends, u1, True)
    step (pends, u, chg) link@(Weak target sources) = do
      -- Check if all sources are in the same group
      (u1, allSame, inputsNode) <- checkSourcesSameGroup sources u
      if allSame
        then do
          -- Merge target to that group
          let
            u2 = union target inputsNode u1
          return (pends, u2, True)
        else
          -- Pending
          return (link : pends, u1, chg)
    step acc _ = Right acc -- Should not happen for weak list

  -- \| Check if all sources are in the same group **nor, has metadata >= 1**
  checkSourcesSameGroup :: [Node] -> UnionFind Node -> Either String (UnionFind Node, Bool, Node)
  checkSourcesSameGroup [] _ = Left "Empty sources"
  checkSourcesSameGroup nodes@(firstNode : _) u =
    let
      -- map by find
      (findResults, lastU) :: ([(Int, Node)], UnionFind Node) =
        foldr
          (\x (acc, currU) -> let (meta, result, newU) = find x currU in ((meta, result) : acc, newU))
          ([], u)
          nodes
      -- filter by meta == 0
      shouldBeSameSource = filter (\(meta, _) -> meta == 0) findResults
      isAllSameSources = case shouldBeSameSource of
        [] -> True
        ((_, node) : rest) -> all (\(_, s) -> s == node) rest
      -- merge all sources to the first source
      mergedU = f nodes
       where
        f [] = lastU
        f [_] = lastU
        f (x : y : rest) = unionWithMetaAddition x y (-1) (f (y : rest))
      (_, root, _) = find firstNode mergedU
    in
      Right (if isAllSameSources then mergedU else lastU, isAllSameSources, root)

  checkConnectedness :: [Node] -> UnionFind Node -> Either String ()
  checkConnectedness [] _ = Left "Empty nodes"
  checkConnectedness (n : ns) uf =
    let
      (rootMeta, root, u1) = find n uf
      checkNode (u, _) x =
        let
          (_, r, u2) = find x u
        in
          if r /= root
            then Left "Graph is disconnected (Need Mix rule)."
            else Right (u2, ())
    in
      if rootMeta /= 0
        then
          Left $ "Graph has " <> show rootMeta <> " unresolved bottom (Maybe need Mix rule)."
        else foldM checkNode (u1, ()) ns >> return ()