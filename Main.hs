module Main where

import Prelude hiding (lookup, Left, Right)
import Control.Monad
import qualified Data.Map as M
import Data.Maybe

type Coord = (Int, Int)

data Level = Level { levelWalls    :: [Wall]
                   , levelHeroes   :: [Hero]
                   , levelMonsters :: [Monster]
                   , levelItems    :: [Item] }


type RenderInfo = (Coord, Char)

class (Position r, Sigil r) => Render r where
    render :: r -> RenderInfo
    render r = (position r, sigil r)


class Position p where
    position :: p -> Coord
    move :: Coord -> p -> p

class Sigil s where
    sigil :: s -> Char


data Hero = Hero { heroPosition :: Coord }

instance Position Hero where
    position = heroPosition
    move coord hero = hero { heroPosition = coord }

instance Sigil Hero where
    sigil = const '@'

instance Render Hero


data Monster = Monster { monsterPosition :: Coord
                       , monsterSigil :: Char }

instance Position Monster where
    position = monsterPosition
    move coord monster = monster { monsterPosition = coord }

instance Sigil Monster where
    sigil = monsterSigil

instance Render Monster


data Wall = Wall { wallPosition :: Coord }

instance Position Wall where
    position = wallPosition
    move coord wall = wall { wallPosition = coord }

instance Sigil Wall where
    sigil = const '#'

instance Render Wall


data Item = Item { itemPosition :: Coord
                 , itemSigil    :: Char }

instance Position Item where
    position = itemPosition
    move coord item = item { itemPosition = coord }

instance Sigil Item where
    sigil = itemSigil

instance Render Item


defaultHero :: Hero
defaultHero = Hero { heroPosition = (2, 2) }

defaultMonster :: Monster
defaultMonster = Monster { monsterPosition = (7, 9)
                         , monsterSigil    = 'k' }

defaultItem :: Item
defaultItem = Item { itemPosition = (7, 8)
                   , itemSigil    = '$' }


squareWall :: Int -> [Wall]
squareWall size = do
    x <- [1 .. size]
    y <- [1 .. size]
    guard $ x == 1 || x == size || y == 1 || y == size
    return Wall { wallPosition = (x, y) }


defaultLevel :: Level
defaultLevel = Level { levelHeroes   = [defaultHero]
                     , levelMonsters = [defaultMonster]
                     , levelWalls    = squareWall 10
                     , levelItems    = [defaultItem] }


drawLevel :: Level -> String
drawLevel level = unlines . map makeRow $ [1 .. 10]
  where makeRow y = map (sigilOrDot y) [1 .. 10]
        sigilOrDot y x = fromMaybe '.' . M.lookup (x, y) $ coordMap
        coordMap = M.unions [ toMap levelHeroes
                            , toMap levelMonsters
                            , toMap levelWalls
                            , toMap levelItems ]
        toMap select = M.fromList . map render . select $ level


data Direction = Stay
               | Up
               | UpRight
               | Right
               | DownRight
               | Down
               | DownLeft
               | Left
               | UpLeft


walk :: Direction -> Coord -> Coord
walk Stay      coord  = coord
walk Up        (x, y) = (x, y - 1)
walk UpRight   (x, y) = (x + 1, y - 1)
walk Right     (x, y) = (x + 1, y)
walk DownRight (x, y) = (x + 1, y + 1)
walk Down      (x, y) = (x, y + 1)
walk DownLeft  (x, y) = (x - 1, y + 1)
walk Left      (x, y) = (x - 1, y)
walk UpLeft    (x, y) = (x - 1, y - 1)


processInput :: Level -> Direction -> Level
processInput level @ (Level { levelHeroes = hero : heroes }) direction =
    level { levelHeroes = move' hero : heroes }
      where move' = move $ walk direction $ position hero


getInput :: IO Direction
getInput = do
    char <- getChar
    return $ case char of
      's' -> Stay
      'w' -> Up
      'e' -> UpRight
      'd' -> Right
      'c' -> DownRight
      'x' -> Down
      'z' -> DownLeft
      'a' -> Left
      'q' -> UpLeft
      _   -> Stay


gameLoop :: Level -> IO ()
gameLoop level = do
    putStr $ drawLevel level
    input <- getInput
    gameLoop $ processInput level input


main :: IO ()
main = gameLoop defaultLevel
