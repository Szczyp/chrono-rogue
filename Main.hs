module Main where

import Prelude hiding (lookup, Left, Right)
import Control.Arrow ((&&&))
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Vinyl hiding (Level)

import Types


defaultMonster :: PlainRec Monster
defaultMonster = position =: (7, 9) <+>
                 sigil    =: 'k'


defaultItem :: PlainRec Item
defaultItem = position =: (7, 8) <+>
              sigil    =: '$'


defaultHero :: PlainRec Hero
defaultHero = position =: (2, 2) <+>
              sigil    =: '@' <+>
              name     =: "HERO!"


squareWall :: Int -> [PlainRec Wall]
squareWall size = do
    x <- [1 .. size]
    y <- [1 .. size]
    guard $ x == 1 || x == size || y == 1 || y == size
    return $ position =: (x, y) <+>
             sigil    =: '#'


defaultLevel :: PlainRec Level
defaultLevel = heroes =: [defaultHero] <+>
               monsters =: [defaultMonster] <+>
               items =: [defaultItem] <+>
               walls =: squareWall 10


drawLevel :: PlainRec Level -> String
drawLevel level = unlines . map makeRow $ [1 .. 10]
  where makeRow y = map (sigilOrDot y) [1 .. 10]
        sigilOrDot y x = fromMaybe '.' . M.lookup (x, y) $ coordMap
        coordMap = M.unions $ map M.fromList renderables
        renderables = map ($ level) [ coords . rGet heroes
                                    , coords . rGet monsters
                                    , coords . rGet items
                                    , coords . rGet walls ]
        coords = map $ rGet position &&& rGet sigil


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


processInput :: PlainRec Level -> Direction -> PlainRec Level
processInput level direction = rMod heroes move level
      where move = map $ rMod position $ walk direction


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


gameLoop :: PlainRec Level -> IO ()
gameLoop level = do
    putStr $ drawLevel level
    input <- getInput
    gameLoop $ processInput level input


main :: IO ()
main = gameLoop defaultLevel
