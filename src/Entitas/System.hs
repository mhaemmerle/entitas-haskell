module Entitas.System where

-- import Entitas.Entity (Entity)
-- import qualified Entitas.Entity as Entity
-- import Entitas.Repository (Repository)
-- import qualified Entitas.Repository as Repository
-- import Entitas.Collection (Collection)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data System = System {
      active :: Bool,
      activate :: (System -> System),
      deactivate :: (System -> System),
      execute :: (System -> System) }

-- empty :: System
-- empty = System True (identity) (identity) (identity)
