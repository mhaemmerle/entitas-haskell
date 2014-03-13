module Entitas.Entity where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe


class (Show c, Ord c, Eq c) => Component c where
  componentType :: c -> String

data Entity a = Entity {
      id :: String,
      creationIndex :: Int,
      components :: Map String a,
      cTypes :: Set String
    } deriving (Eq, Ord, Show)

-- |The 'hasComponentOfType' function checks for component type membership in
-- an Entity's components
hasComponentOfType :: Entity a -> String -> Bool
hasComponentOfType entity cType = Set.member cType $ cTypes entity

hasComponentsOfTypes :: Entity a -> Set String -> Bool
hasComponentsOfTypes entity cTypesIn = Set.isSubsetOf cTypesIn $ cTypes entity

componentOfType :: Component a => Entity a -> String -> Maybe a
componentOfType entity cType = Map.lookup cType $ components entity

containsComponent :: Component a => Entity a -> a -> Bool
containsComponent entity component = isJust $ componentOfType entity $ componentType component

-- modifyComponents :: (Map String Component -> Map String Component) -> (Entity -> Entity)


makeEntity :: Component a => [a] -> Entity a
makeEntity cs = Entity "someId" 1 components types
    where components = Map.fromList $ zip typeList cs  
          types = Set.fromList typeList
          typeList = map componentType cs 


addComponent :: Component a => Entity a -> a -> Entity a
addComponent entity component =
    if hasComponentOfType entity $ componentType component then entity else
        (entity{components = Map.insert (componentType component) component $ components entity,
                cTypes = Set.insert (componentType component) $ cTypes entity})

exchangeComponent :: Component a => Entity a -> a -> Entity a
exchangeComponent entity component =
    addComponent entity component

removeComponentOfType :: Entity a -> String -> Entity a
removeComponentOfType entity cType =
    if not $ hasComponentOfType entity cType then entity else
        (entity{components = Map.delete cType (components entity),
                cTypes = Set.delete cType (cTypes entity)})
