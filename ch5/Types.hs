module Types where

import qualified Symbol

type Unique = Integer

data Ty = TInt
        | TString
        | TRecord [(Symbol.Symbol, Ty)] Unique
        | TArray Ty Unique
        | TNil
        | TUnit
        | TName Symbol.Symbol (Maybe Ty)
        deriving (Show, Eq)
