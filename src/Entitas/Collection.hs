module Entitas.Collection where

import Entitas.Entity (Entity, creationIndex)
import Entitas.Matcher (allOfSet)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function

data Collection = Collection {
      matcher :: (Set String -> Bool),
      entities :: Map Int Entity
    }

instance Ord Collection where
    compare = compare `on` entities

instance Eq Collection where
    (Collection _ e1) == (Collection _ e2) = e1 == e2

instance Show Collection where
    show (Collection _ e) = show e

initWithMatcher :: (Set String -> Bool) -> Collection
initWithMatcher matcher = Collection matcher $ Map.empty

initWithTypes :: Set String -> Collection
initWithTypes cTypes = initWithMatcher $ allOfSet cTypes
                       
getEntities :: Collection -> [Entity]
getEntities collection = Map.elems $ entities collection

getMatcher :: Collection -> (Set String -> Bool)
getMatcher collection = matcher collection

addEntity :: Collection -> Entity -> Collection
addEntity collection entity =
    if Map.member (creationIndex entity) (entities collection) then collection else
        (collection{entities = Map.insert (creationIndex entity) entity (entities collection)})

exchangeEntity :: Collection -> Entity -> Collection
exchangeEntity collection entity =
    if Map.member (creationIndex entity) (entities collection)
    then collection
    else addEntity collection entity

removeEntity :: Collection -> Entity -> Collection
removeEntity collection entity =
    if Map.notMember (creationIndex entity) (entities collection) then collection
    else (collection{entities = Map.delete (creationIndex entity) (entities collection)})
