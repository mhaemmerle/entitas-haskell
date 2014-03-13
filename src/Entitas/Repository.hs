module Entitas.Repository where

import Entitas.Entity (Entity, Component, creationIndex, cTypes)
import Entitas.Matcher (allOfSet, just)
import Entitas.Collection (Collection, initWithMatcher, addEntity, getMatcher)
import qualified Entitas.Collection as Collection

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (Maybe)

data Repository a = Repository {
      entities :: Map Int (Entity a),
      collectionsForType :: Map String (Set (Collection a)),
      currentIndex :: Int
    } deriving (Eq, Ord, Show)


emptyRepository :: Repository a
emptyRepository = Repository Map.empty Map.empty 0

allEntities :: Repository a -> [Entity a]
allEntities repository = Map.elems $ entities repository

addEntity :: Repository a -> Entity a -> Repository a
addEntity repository entity =
    repository{entities = Map.insert (currentIndex repository) newEntity (entities repository),
               currentIndex = (currentIndex repository) + 1}
        where
          newEntity = entity{creationIndex = (currentIndex repository)}

removeEntity :: Repository a -> Entity a -> Repository a
removeEntity repository entity = repository

containsEntity :: Repository a -> Entity a -> Bool
containsEntity repository entity = True

internalCollectionsForType :: Repository a -> String -> Set (Collection a)
internalCollectionsForType repository cType =
    case Map.lookup cType (collectionsForType repository) of
      Just value -> value
      Nothing -> Set.empty

matchFilter :: (Set String -> Bool) -> Entity a -> Bool
matchFilter matcher entity = matcher $ cTypes entity

entitiesForMatcher :: Repository a -> (Set String -> Bool) -> [Entity a]
entitiesForMatcher repository matcher =
    Map.elems $ Map.filter (matchFilter matcher) (entities repository)

addComponent :: (Component a) => Repository a -> String -> Entity a -> Repository a
addComponent repository cType entity =
    let r1 = repository{entities = Map.insert (creationIndex entity) entity (entities repository)}
        cft = internalCollectionsForType r1 cType
        f = (\c -> if (getMatcher c) (cTypes entity) then (Collection.addEntity c entity) else c)
    in r1{collectionsForType =
              Map.insert cType (if (Set.size cft == 0)
                                then (Set.singleton (f (initWithMatcher $ just cType)))
                                else (Set.map f cft)) (collectionsForType r1)}

exchangeComponent :: (Component a) => Repository a -> String -> Entity a -> Repository a
exchangeComponent repository cType entity =
    let f = (\c -> if (getMatcher c) (cTypes entity)
                   then (Collection.exchangeEntity c entity)
                   else c)
    in repository{collectionsForType = Map.adjust (Set.map f) cType (collectionsForType repository)}

removeComponent :: (Component a) => Repository a -> String -> Entity a -> Repository a
removeComponent repository cType entity =
    let ct = Set.insert cType (cTypes entity)
        f = (\c -> if (getMatcher c) ct && (not $ (getMatcher c) (cTypes entity))
                   then (Collection.removeEntity c entity)
                   else c)
    in repository{collectionsForType = Map.adjust (Set.map f) cType (collectionsForType repository)}
