module Graph.UnionFind
  ( UnionFind
  , emptyUF
  , find
  , union
  , addMeta
  , unionWithMetaAddition
  )
where

import qualified Data.Map as M

-- Simple Union-Find using a Map
-- Keys are nodes, Values are parents.
-- If a key is missing or maps to itself, it's a root.
newtype UnionFind node = UnionFind (M.Map node (Int, node)) deriving (Show, Eq)

emptyUF :: UnionFind node
emptyUF = UnionFind M.empty

-- Find the representative of a node, with path compression (simulated by recursion).
find :: (Ord node) => node -> UnionFind node -> (Int, node, UnionFind node)
find x uf@(UnionFind m) =
  case M.lookup x m of
    Nothing -> (0, x, uf) -- efficient default: implicit self-loop
    Just (meta, parent) ->
      if parent == x
        then (meta, x, uf)
        else
          let
            (rootMeta, root, newUF) = find parent uf
            -- Path compression: update direct parent to root
            UnionFind newM = newUF
          in
            (rootMeta, root, UnionFind (M.insert x (rootMeta, root) newM))

-- Union two nodes.
-- Returns newUF.
union :: (Ord node) => node -> node -> UnionFind node -> UnionFind node
union x y uf =
  let
    (meta1, rootX, uf1) = find x uf
    (meta2, rootY, uf2) = find y uf1
  in
    if rootX == rootY
      then uf2
      else
        let
          UnionFind m = uf2
        in
          UnionFind (M.insert rootY (meta1 + meta2, rootY) $ M.insert rootX (meta1 + meta2, rootY) m)

addMeta :: (Ord node) => node -> Int -> UnionFind node -> UnionFind node
addMeta x meta uf =
  let
    (rootMeta, root, uf1) = find x uf
    UnionFind m = uf1
  in
    UnionFind (M.insert root (rootMeta + meta, root) m)

unionWithMetaAddition :: (Ord node) => node -> node -> Int -> UnionFind node -> UnionFind node
unionWithMetaAddition x y meta uf =
  let
    (meta1, rootX, uf1) = find x uf
    (meta2, rootY, uf2) = find y uf1
  in
    if rootX == rootY
      then uf2
      else
        let
          UnionFind m = uf2
        in
          UnionFind (M.insert rootY (meta1 + meta2 + meta, rootY) $ M.insert rootX (meta1 + meta2 + meta, rootY) m)