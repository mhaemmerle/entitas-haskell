module Entitas.Matcher where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import Data.Hashable (hash)


type Matcher = (Set String -> Bool)

allMatching :: Set String -> Matcher
allMatching a b = Set.isSubsetOf a b

anyMatching :: Set String -> Matcher
anyMatching a b = not $ Set.null $ Set.intersection a b

equal :: Set String -> Set String -> Bool
equal a b = a == b

allOf :: [String] -> Matcher
allOf a b = allMatching b $ Set.fromList a

allOfSet :: Set String -> Matcher
allOfSet a b = allMatching a b

anyOf :: [String] -> Matcher
anyOf a b = anyMatching b $ Set.fromList a

anyOfSet :: Set String -> Matcher
anyOfSet a b = anyMatching a b

just :: String -> Matcher
just a b = allMatching (Set.singleton a) b

toKey :: String -> [String] -> Int
toKey a b = hash $ concat $ a : b
