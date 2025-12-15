module Graph.Types
  ( Node
  , Link (..)
  )
where

type Node = String

data Link
  = Strong [Node] -- Unconditional merge of all nodes (Tensor, Cut, etc.)
  | Weak Node [Node] -- Conditional merge: inputs -> target (Par)
  deriving (Show, Eq)
