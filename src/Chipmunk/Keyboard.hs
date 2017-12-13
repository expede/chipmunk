{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Chipmunk.Keyboard where

import           ClassyPrelude hiding (Down)

{- Keyboard

The keyboard was originally laid out as a 16-key hex keypad:

            +---+---+---+---+
            | 1 | 2 | 3 | C |
            +---+---+---+---+
            | 4 | 5 | 6 | D |
            +---+---+---+---+
            | 7 | 8 | 9 | E |
            +---+---+---+---+
            | A | 0 | B | F |
            +---+---+---+---+

This needs to be emulated for modern keyboards

-}

-- User configurable keymap
data Config = Config
  { char0 :: Char
  , char1 :: Char
  , char2 :: Char
  , char3 :: Char
  , char4 :: Char
  , char5 :: Char
  , char6 :: Char
  , char7 :: Char
  , char8 :: Char
  , char9 :: Char
  , charA :: Char
  , charB :: Char
  , charC :: Char
  , charD :: Char
  , charE :: Char
  , charF :: Char
  }

-- Virtual keyboard state
data State = State
  { pos0 :: Position
  , pos1 :: Position
  , pos2 :: Position
  , pos3 :: Position
  , pos4 :: Position
  , pos5 :: Position
  , pos6 :: Position
  , pos7 :: Position
  , pos8 :: Position
  , pos9 :: Position
  , posA :: Position
  , posB :: Position
  , posC :: Position
  , posD :: Position
  , posE :: Position
  , posF :: Position
  }

-- Pressedness of a single key
data Position = Down | Up

refresh :: [Char] -> Config -> State
refresh input (Config {..}) =
  State
    { pos0 = checkState char0
    , pos1 = checkState char1
    , pos2 = checkState char2
    , pos3 = checkState char3
    , pos4 = checkState char4
    , pos5 = checkState char5
    , pos6 = checkState char6
    , pos7 = checkState char7
    , pos8 = checkState char8
    , pos9 = checkState char9
    , posA = checkState charA
    , posB = checkState charB
    , posC = checkState charC
    , posD = checkState charD
    , posE = checkState charE
    , posF = checkState charF
    }

  where
    checkState char =
      if elem char input
         then Down
         else Up
