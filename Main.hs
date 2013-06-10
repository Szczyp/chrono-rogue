module Main where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

type Coord = (Int, Int)

data Level = Level { levelWalls  :: [Wall]
                   , levelHeroes :: [Hero]
                   , levelItems  :: [Item] }


class Position p where
    getPosition :: p -> Coord

class Sigil s where
    getSigil :: s -> Char


data Hero = Hero { heroPosition :: Coord }

instance Position Hero where
    getPosition = heroPosition

instance Sigil Hero where
    getSigil = const '@'


data Wall = Wall { wallPosition :: Coord }

instance Position Wall where
    getPosition = wallPosition

instance Sigil Wall where
    getSigil = const '#'


data Item = Item { itemPosition :: Coord
                 , itemSigil    :: Char }

instance Position Item where
    getPosition = itemPosition

instance Sigil Item where
    getSigil = itemSigil


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
drawLevel level = unlines . map row $ [1 .. 10]
  where row y = map (sigil y) [1 .. 10]
        sigil y x = fromMaybe ' ' . getFirst . mconcat . map (First . ($ (x, y))) $ lookups
        lookups = [ lookup levelWalls
                  , lookup levelHeroes
                  , lookup levelItems ]
        lookup select coord = getSigil <$> M.lookup coord (toMap . select $ level)
        toMap xs = M.fromList $ map (getPosition &&& id) xs

main :: IO ()
main = putStr $ drawLevel defaultLevel
