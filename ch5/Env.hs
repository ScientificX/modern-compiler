module Env where

import qualified Types
import qualified Symbol

data EnvEntry = VarEntry Types.Ty
              | FunEntry [Types.Ty] Types.Ty
              deriving (Show)

fromList xs = foldl (\t (s, a) -> Symbol.enter t s a) Symbol.empty xs

baseTEnv :: Symbol.Table Types.Ty
baseTEnv = fromList [ ("int",    Types.TInt)
                    , ("string", Types.TString) ]

baseVEnv :: Symbol.Table EnvEntry
baseVEnv = fromList [ ("print",     FunEntry [Types.TString]                           Types.TUnit)
                    , ("flush",     FunEntry []                                        Types.TUnit)
                    , ("getchar",   FunEntry []                                        Types.TString)
                    , ("ord",       FunEntry [Types.TString]                           Types.TInt)
                    , ("chr",       FunEntry [Types.TInt]                              Types.TString)
                    , ("size",      FunEntry [Types.TString]                           Types.TInt)
                    , ("substring", FunEntry [Types.TString, Types.TInt, Types.TInt]   Types.TInt)
                    , ("concat",    FunEntry [Types.TString, Types.TString]            Types.TString)
                    , ("not",       FunEntry [Types.TInt]                              Types.TInt)
                    , ("exit",      FunEntry [Types.TInt]                              Types.TUnit) ]
