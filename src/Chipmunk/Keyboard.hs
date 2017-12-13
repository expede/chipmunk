{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Keyboard where

import           ClassyPrelude

{- Keyboard

The keyboard is laid out as a 16-key dex keypad:

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
