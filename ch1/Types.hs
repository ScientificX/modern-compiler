module Types (Id(..), BinOp(..), Stm(..), Exp(..)) where

type Id = String

data BinOp = Plus | Minus | Times | Divide deriving (Show)

data Stm = CompoundStm Stm Stm
          | AssignStm Id Exp
          | PrintStm [Exp]
          deriving (Show)

data Exp = IdExp Id
          | NumExp Integer
          | OpExp Exp BinOp Exp
          | EseqExp Stm Exp
          deriving (Show)
