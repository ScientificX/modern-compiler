module Env where

import qualified Types
import qualified Symbol
import qualified Translate
import qualified Temp

data EnvEntry = VarEntry Types.Ty Translate.Access
              | FunEntry [Types.Ty] Types.Ty Translate.Level Temp.Label
              deriving (Show)

fromList xs = foldl (\t (s, a) -> Symbol.enter t s a) Symbol.empty xs

baseTEnv :: Symbol.Table Types.Ty
baseTEnv = fromList [ ("int",    Types.TInt)
                    , ("string", Types.TString) ]

baseVEnv :: Symbol.Table EnvEntry
baseVEnv = fromList [ ("print",     FunEntry [Types.TString]                           Types.TUnit   Translate.outermost (Temp.namedLabel "print"))
                    , ("flush",     FunEntry []                                        Types.TUnit   Translate.outermost (Temp.namedLabel "flush"))
                    , ("getchar",   FunEntry []                                        Types.TString Translate.outermost (Temp.namedLabel "getchar"))
                    , ("ord",       FunEntry [Types.TString]                           Types.TInt    Translate.outermost (Temp.namedLabel "org"))
                    , ("chr",       FunEntry [Types.TInt]                              Types.TString Translate.outermost (Temp.namedLabel "chr"))
                    , ("size",      FunEntry [Types.TString]                           Types.TInt    Translate.outermost (Temp.namedLabel "size"))
                    , ("substring", FunEntry [Types.TString, Types.TInt, Types.TInt]   Types.TInt    Translate.outermost (Temp.namedLabel "substring"))
                    , ("concat",    FunEntry [Types.TString, Types.TString]            Types.TString Translate.outermost (Temp.namedLabel "concat"))
                    , ("not",       FunEntry [Types.TInt]                              Types.TInt    Translate.outermost (Temp.namedLabel "not"))
                    , ("exit",      FunEntry [Types.TInt]                              Types.TUnit   Translate.outermost (Temp.namedLabel "exit"))
                    ]
