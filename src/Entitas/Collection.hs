module Entitas.Collection where

import Entitas.Entity (Entity, Component, creationIndex)
import Entitas.Matcher (allOfSet)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function

data Collection a = Collection {
      matcher :: (Set String -> Bool),
      entities :: Map Int (Entity a) 
    }

instance (Component a) => Ord (Collection a) where
    compare = compare `on` entities

instance (Component a) => Eq (Collection a) where
    (Collection _ e1) == (Collection _ e2) = e1 == e2

instance (Component a) => Show (Collection a) where
    show (Collection _ e) = show e

initWithMatcher :: (Set String -> Bool) -> Collection a
initWithMatcher matcher = Collection matcher $ Map.empty

initWithTypes :: Set String -> Collection a
initWithTypes cTypes = initWithMatcher $ allOfSet cTypes
                       
getEntities :: Collection a -> [Entity a]
getEntities collection = Map.elems $ entities collection

getMatcher :: Collection a -> (Set String -> Bool)
getMatcher collection = matcher collection

addEntity :: Collection a -> Entity a -> Collection a
addEntity collection entity =
    if Map.member (creationIndex entity) (entities collection) then collection else
        (collection{entities = Map.insert (creationIndex entity) entity (entities collection)})

exchangeEntity :: Collection a -> Entity a -> Collection a
exchangeEntity collection entity =
    if Map.member (creationIndex entity) (entities collection)
    then collection
    else addEntity collection entity

removeEntity :: Collection a -> Entity a -> Collection a
removeEntity collection entity =
    if Map.notMember (creationIndex entity) (entities collection) then collection
    else (collection{entities = Map.delete (creationIndex entity) (entities collection)})
