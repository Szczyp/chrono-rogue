module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

type Coord = (Int, Int)

type LevelMap = M.Map Coord

data Item = Item {}
                 deriving (Show)

data Wall = Wall {}
                 deriving (Show)

data Hero = Hero { heroName :: String }
                 deriving (Show)

data Level = Level { levelItems  :: LevelMap Item
                   , levelHeroes :: LevelMap Hero
                   , levelWalls  :: LevelMap Wall }
                   deriving (Show)

data World = World { worldLevels :: [Level]
                   , worldHeroes :: [Hero] }

makeSquare :: Int -> [(Coord, Wall)]
makeSquare size = do
    x <- [1 .. size]
    y <- [1 .. size]
    guard $ x == 1 || x == size || y == 1 || y == size
    return ((x, y), Wall)

emptyLevel :: Level
emptyLevel = Level { levelItems = M.empty
                   , levelHeroes = M.fromList [((2, 2), Hero { heroName = "hero!" })]
                   , levelWalls = M.fromList $ makeSquare 10 }

drawLevel :: Level -> String
drawLevel level = unlines . map row $ [1 .. 10]
  where row y = map char [1 .. 10]
          where char x = fromMaybe ' ' . getFirst . mconcat . map (First . ($ (x, y))) $ drawFs
        lookup select char coord = const char <$> M.lookup coord (select level)
        drawFs = [ lookup levelWalls '#'
                 , lookup levelHeroes '@'
                 , lookup levelItems '$' ]

main :: IO ()
main = putStr $ drawLevel emptyLevel
