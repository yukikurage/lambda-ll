module Graph.UnionFind
  ( UnionFind
  , emptyUF
  , find
  , union
  )
where

import qualified Data.Map as M
import Graph.Types (Node)

-- Simple Union-Find using a Map
-- Keys are nodes, Values are parents.
-- If a key is missing or maps to itself, it's a root.
newtype UnionFind = UnionFind (M.Map Node Node) deriving (Show, Eq)

emptyUF :: UnionFind
emptyUF = UnionFind M.empty

-- Find the representative of a node, with path compression (simulated by recursion).
find :: Node -> UnionFind -> (Node, UnionFind)
find x uf@(UnionFind m) =
  case M.lookup x m of
    Nothing -> (x, uf) -- efficient default: implicit self-loop
    Just parent ->
      if parent == x
        then (x, uf)
        else
          let
            (root, newUF) = find parent uf
            -- Path compression: update direct parent to root
            UnionFind newM = newUF
          in
            (root, UnionFind (M.insert x root newM))

-- Union two nodes.
-- Returns newUF.
union :: Node -> Node -> UnionFind -> UnionFind
union x y uf =
  let
    (rootX, uf1) = find x uf
    (rootY, uf2) = find y uf1
  in
    if rootX == rootY
      then uf2
      else
        let
          UnionFind m = uf2
        in
          UnionFind (M.insert rootX rootY m)
