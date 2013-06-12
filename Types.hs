{-# LANGUAGE DataKinds #-}

module Types where

import Data.Vinyl

position :: "position" ::: Coord
position = Field

sigil  :: "sigil"    ::: Char
sigil    = Field

name :: "name" ::: String
name = Field

heroes  :: "heroes" ::: [PlainRec Hero]
heroes = Field

monsters  :: "monsters" ::: [PlainRec Monster]
monsters = Field

items  :: "items" ::: [PlainRec Item]
items = Field

walls  :: "walls" ::: [PlainRec Wall]
walls = Field


type Coord = (Int, Int)

type Renderable = ["position" ::: Coord, "sigil" ::: Char]

type Monster = ["position" ::: Coord, "sigil" ::: Char]

type Item = ["position" ::: Coord, "sigil" ::: Char]

type Hero = ["position" ::: Coord, "sigil" ::: Char, "name" ::: String]

type Wall = ["position" ::: Coord, "sigil" ::: Char]

type Level = [ "heroes"   ::: [PlainRec Hero]
             , "monsters" ::: [PlainRec Monster]
             , "items"    ::: [PlainRec Item]
             , "walls"    ::: [PlainRec Wall] ]


data Direction = Stay
               | Up
               | UpRight
               | Right
               | DownRight
               | Down
               | DownLeft
               | Left
               | UpLeft

