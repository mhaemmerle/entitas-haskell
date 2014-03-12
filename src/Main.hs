module Main(main) where

import Entitas.Entity (Entity)
import qualified Entitas.Entity as Entity
import Entitas.Repository (Repository)
import qualified Entitas.Repository as Repository
import Entitas.Collection (Collection)

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

newComponent :: String -> Entity.Component
newComponent cType = Entity.Component cType

-- move to entity module
newEntity :: String -> Int -> Map String Entity.Component -> Set String -> Entity
newEntity id creationIndex components cTypes =
    Entity.Entity id creationIndex components cTypes

createTestEntity :: String -> Int -> String -> Entity
createTestEntity id creationIndex cType = newEntity id creationIndex components cTypes
    where components = Map.insert cType (newComponent cType) $ Map.empty
          cTypes = Set.insert cType $ Set.empty

-- move to repository module
newRepository :: Map Int Entity -> Map String (Set Collection) -> Int -> Repository
newRepository entities collectionsForType currentIndex =
    Repository.Repository entities collectionsForType currentIndex

createTestRepository :: Repository
createTestRepository =
    newRepository (Map.empty) (Map.empty) 0

main :: IO ()
main = do
  putStrLn $ show $ createTestRepository
  putStrLn $ show $ createTestEntity "id1" 0 "c1"
