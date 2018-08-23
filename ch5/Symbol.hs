module Symbol where

import qualified Data.Map.Strict as Map

type Symbol = String
type Table = Map.Map Symbol


empty = Map.empty

enter t s a = Map.insert s a t

look t s = Map.lookup s t
