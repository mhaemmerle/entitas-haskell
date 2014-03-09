module Entitas.Matcher where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import Data.Hashable (hash)

allMatching :: Set String -> Set String -> Bool
allMatching a b = Set.isSubsetOf a b

anyMatching :: Set String -> Set String -> Bool
anyMatching a b = not $ Set.null $ Set.intersection a b

equal :: Set String -> Set String -> Bool
equal a b = a == b

allOf :: Set String -> [String] -> Bool
allOf a b = allMatching a $ Set.fromList b

allOfSet :: Set String -> Set String -> Bool
allOfSet a b = allMatching a b

anyOf :: Set String -> [String] -> Bool
anyOf a b = anyMatching a $ Set.fromList b

anyOfSet :: Set String -> Set String -> Bool
anyOfSet a b = anyMatching a b

just :: String -> Set String -> Bool
just a b = allMatching (Set.singleton a) b

toKey :: String -> [String] -> Int
toKey a b = hash $ concat $ a : b
