module Main where

import Prelude hiding (lookup)
import Control.Monad
import qualified Data.Map as M
import Data.Maybe

type Coord = (Int, Int)

data Level = Level { levelWalls  :: [Wall]
                   , levelHeroes :: [Hero]
                   , levelItems  :: [Item] }


type RenderInfo = (Coord, Char)

class (Position renderable, Sigil renderable) => Renderable renderable where
    render :: renderable -> RenderInfo
    render renderable = (position renderable, sigil renderable)


class Position p where
    position :: p -> Coord

class Sigil s where
    sigil :: s -> Char


data Hero = Hero { heroPosition :: Coord }

instance Position Hero where
    position = heroPosition

instance Sigil Hero where
    sigil = const '@'

instance Renderable Hero


data Wall = Wall { wallPosition :: Coord }

instance Position Wall where
    position = wallPosition

instance Sigil Wall where
    sigil = const '#'

instance Renderable Wall


data Item = Item { itemPosition :: Coord
                 , itemSigil    :: Char }

instance Position Item where
    position = itemPosition

instance Sigil Item where
    sigil = itemSigil

instance Renderable Item


defaultHero :: Hero
defaultHero = Hero { heroPosition = (2, 2) }


defaultItem :: Item
defaultItem = Item { itemPosition = (7, 8)
                   , itemSigil = '$' }


squareWall :: Int -> [Wall]
squareWall size = do
    x <- [1 .. size]
    y <- [1 .. size]
    guard $ x == 1 || x == size || y == 1 || y == size
    return Wall { wallPosition = (x, y) }


defaultLevel :: Level
defaultLevel = Level { levelHeroes = [defaultHero]
                     , levelWalls = squareWall 10
                     , levelItems = [defaultItem] }


drawLevel :: Level -> String
drawLevel level = unlines . map makeRow $ [1 .. 10]
  where makeRow y = map (sigilOrDot y) [1 .. 10]
        sigilOrDot y x = fromMaybe '.' . M.lookup (x, y) $ coordMap
        coordMap = M.unions [toMap levelHeroes, toMap levelWalls, toMap levelItems]
        toMap select = M.fromList . map render . select $ level


main :: IO ()
main = putStr $ drawLevel defaultLevel
