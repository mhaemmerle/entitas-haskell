module Main(main) where

import Entitas.Entity
import qualified Entitas.Entity as Entity
import Entitas.Repository
import qualified Entitas.Repository as Repository
import Entitas.Collection (Collection)

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Entitas.Matcher

import UI.NCurses
import Control.Monad


data ComponentData = Position (Integer, Integer) 
      | Player
    deriving (Show, Eq, Ord)

-- very dirty / very convenient. Should probably not make it into production code
instance Component ComponentData where
  componentType c = (words $ show c) !! 0
   

-- move to entity module
-- newEntity :: Int -> Map String a -> Set String -> Entity a
-- newEntity creationIndex components cTypes =
--     Entity.Entity "someId" creationIndex components cTypes

-- createTestEntity :: (Component a) => a -> Int -> Entity a
-- createTestEntity component creationIndex = newEntity creationIndex components cTypes
--     where components = Map.insert (componentType component) component $ Map.empty
--           cTypes = Set.insert (componentType component) $ Set.empty

-- move to repository module
-- newRepository :: Map Int (Entity a) -> Map String (Set (Collection a)) -> Int -> Repository a
-- newRepository entities collectionsForType currentIndex =
--     Repository.Repository entities collectionsForType currentIndex
-- 
-- createTestRepository :: Repository a
-- createTestRepository =
--     newRepository (Map.empty) (Map.empty) 0

-- main :: IO ()
-- main = do
--   putStrLn $ show $ createTestRepository
--   putStrLn $ show $ createTestEntity "id1" 0 "c1"



main :: IO ()
main = runCurses $ do
  
  let playerE = makeEntity [Position (10, 10)]
  let r = emptyRepository
  let r' = addEntity r playerE  
  
  setEcho False
  setCursorMode CursorInvisible
  w <- newWindow 20 40 0 0

  updateWindow w $ do
    moveCursor 0 0
    drawBox (Just $ Glyph '|' []) (Just $ Glyph '-' [])
  render

  renderLoop w r'

  return ()


renderLoop :: Window -> Repository ComponentData -> Curses ()
renderLoop w r = do 
  
  let es = (entitiesForMatcher r $ allOfSet $ Set.singleton "Position")::[Entity ComponentData]
  let e = es !! 0
  let Just (Position (x, y)) = componentOfType e "Position"
  
  loop y x where 
    
    loop y x = do
      ev <- getEvent w $ Just 10
      updateScreen w y x
      case ev of
        Nothing -> loop y x
        Just ev' -> case ev' of
          EventCharacter 'q' -> return ()
          EventCharacter 'j' -> loop (y+1) x
          EventCharacter 'k' -> loop (y-1) x
          EventCharacter 'h' -> loop y (x-1)
          EventCharacter 'l' -> loop y (x+1)
          otherwise -> loop y x


updateScreen :: Window -> Integer -> Integer -> Curses ()
updateScreen w y x = do
  updateWindow w $ do
    clear w
    moveCursor y x
    drawString "@"
  render

clear :: Window -> Update ()
clear w = forM_ [1..18] (\l -> (moveCursor l 1) >> (drawString $ replicate 38 ' '))



