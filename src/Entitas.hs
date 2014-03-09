module Main(main) where

import Entitas.Entity (Entity)
import qualified Entitas.Entity as Entity

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

newComponent :: String -> Entity.Component
newComponent cType = Entity.Component cType

newEntity :: String -> Int -> Map String Entity.Component -> Set String -> Entity
newEntity id creationIndex components cTypes =
    Entity.Entity id creationIndex components cTypes

createTestEntity :: String -> Int -> String -> Entity
createTestEntity id creationIndex cType = newEntity id creationIndex components cTypes
    where components = Map.insert cType (newComponent cType) $ Map.empty
          cTypes = Set.insert cType $ Set.empty

main :: IO ()
main = do
  putStrLn $ show $ createTestEntity "id1" 0 "c1"
