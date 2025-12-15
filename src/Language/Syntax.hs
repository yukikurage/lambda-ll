module Language.Syntax
  ( Name
  , Type (..)
  , Term (..)
  , Stmt (..)
  , Program (..)
  , TopLevel (..)
  , Pattern (..)
  , TypedPattern (..)
  , printType
  )
where

import Data.List (intercalate)

type Name = String

data Type
  = TAtom Name
  | TDual Type
  | TTensor [Type]
  | TPar [Type]
  | TFun [Type] Type -- Sugar: (A, B) => C. Stores as ([A, B], C).
  deriving (Show, Eq)

printType :: Type -> String
printType t = case t of
  TAtom n -> n
  TDual t' -> "~" ++ printType t'
  TTensor ts -> "[" ++ intercalate ", " (map printType ts) ++ "]"
  TPar ts -> "{" ++ intercalate ", " (map printType ts) ++ "}"
  TFun args ret -> "(" ++ intercalate ", " (map printType args) ++ ") => " ++ printType ret

data Pattern
  = PVar Name
  | PTensor [Pattern]
  | PPar [Pattern]
  deriving (Show, Eq)

data TypedPattern
  = TPVar Name Type
  | TPTensor [TypedPattern]
  | TPPar [TypedPattern]
  deriving (Show, Eq)

data Term
  = Var Name
  | Tensor [Term]
  | Par [Term]
  | Block [Stmt] Term
  | Lambda [TypedPattern] Term -- Sugar: (x:T) => t
  | App Term [Term] -- Sugar: f(t, u)
  deriving (Show, Eq)

data Stmt
  = Let Pattern Term
  | Intro Name Name Type -- intro arg1, pToQ : P => Q
  | Elim Term Term -- elim p, pInv
  deriving (Show, Eq)

data Program = Program [TopLevel] deriving (Show, Eq)

data TopLevel
  = TypeDef Name Type
  | GlobalLet Name (Maybe Type) Term
  deriving (Show, Eq)
