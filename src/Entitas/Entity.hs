module Entitas.Entity where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

data Component = Component {
      cType :: String
    } deriving (Eq, Ord, Show)
               
data Entity = Entity {
      id :: String,
      creationIndex :: Int,
      components :: Map String Component,
      cTypes :: Set String
    } deriving (Eq, Ord, Show)

-- |The 'hasComponentOfType' function checks for component type membership in
-- an Entity's components
hasComponentOfType :: Entity -> String -> Bool
hasComponentOfType entity cType = Set.member cType $ cTypes entity

hasComponentsOfTypes :: Entity -> Set String -> Bool
hasComponentsOfTypes entity cTypesIn = Set.isSubsetOf cTypesIn $ cTypes entity

componentOfType :: Entity -> String -> Maybe Component
componentOfType entity cType = Map.lookup cType $ components entity

containsComponent :: Entity -> Component -> Bool
containsComponent entity component = isJust $ componentOfType entity $ cType component

-- modifyComponents :: (Map String Component -> Map String Component) -> (Entity -> Entity)

addComponent :: Entity -> Component -> Entity
addComponent entity component =
    if hasComponentOfType entity $ cType component then entity else
        (entity{components = Map.insert (cType component) component $ components entity,
                cTypes = Set.insert (cType component) $ cTypes entity})

exchangeComponent :: Entity -> Component -> Entity
exchangeComponent entity component =
    addComponent entity component

removeComponentOfType :: Entity -> String -> Entity
removeComponentOfType entity cType =
    if not $ hasComponentOfType entity cType then entity else
        (entity{components = Map.delete cType (components entity),
                cTypes = Set.delete cType (cTypes entity)})
