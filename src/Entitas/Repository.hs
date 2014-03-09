module Entitas.Repository where

import Entitas.Entity (Entity, creationIndex, cTypes)
import Entitas.Matcher (allOfSet, just)
import Entitas.Collection (Collection, initWithMatcher, addEntity, getMatcher)
import qualified Entitas.Collection as Collection

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (Maybe)

data Repository = Repository {
      entities :: Map Int Entity,
      collectionsForType :: Map String (Set Collection),
      currentIndex :: Int
    } deriving (Eq, Ord, Show)

allEntities :: Repository -> [Entity]
allEntities repository = Map.elems $ entities repository

addEntity :: Repository -> Entity -> Repository
addEntity repository entity =
    repository{entities = Map.insert (currentIndex repository) newEntity (entities repository),
               currentIndex = (currentIndex repository) + 1}
        where
          newEntity = entity{creationIndex = (currentIndex repository)}

removeEntity :: Repository -> Entity -> Repository
removeEntity repository entity = repository

containsEntity :: Repository -> Entity -> Bool
containsEntity repository entity = True

internalCollectionsForType :: Repository -> String -> Set Collection
internalCollectionsForType repository cType =
    case Map.lookup cType (collectionsForType repository) of
      Just value -> value
      Nothing -> Set.empty

matchFilter :: (Set String -> Bool) -> Entity -> Bool
matchFilter matcher entity = matcher $ cTypes entity

entitiesForMatcher :: Repository -> (Set String -> Bool) -> [Entity]
entitiesForMatcher repository matcher =
    Map.elems $ Map.filter (matchFilter matcher) (entities repository)

addComponent :: Repository -> String -> Entity -> Repository
addComponent repository cType entity =
    let r1 = repository{entities = Map.insert (creationIndex entity) entity (entities repository)}
        cft = internalCollectionsForType r1 cType
        f = (\c -> if (getMatcher c) (cTypes entity) then (Collection.addEntity c entity) else c)
    in r1{collectionsForType =
              Map.insert cType (if (Set.size cft == 0)
                                then (Set.singleton (f (initWithMatcher $ just cType)))
                                else (Set.map f cft)) (collectionsForType r1)}

exchangeComponent :: Repository -> String -> Entity -> Repository
exchangeComponent repository cType entity =
    let f = (\c -> if (getMatcher c) (cTypes entity)
                   then (Collection.exchangeEntity c entity)
                   else c)
    in repository{collectionsForType = Map.adjust (Set.map f) cType (collectionsForType repository)}

removeComponent :: Repository -> String -> Entity -> Repository
removeComponent repository cType entity =
    let ct = Set.insert cType (cTypes entity)
        f = (\c -> if (getMatcher c) ct && (not $ (getMatcher c) (cTypes entity))
                   then (Collection.removeEntity c entity)
                   else c)
    in repository{collectionsForType = Map.adjust (Set.map f) cType (collectionsForType repository)}
