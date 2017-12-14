{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Display where

import           ClassyPrelude

{- Display

==============
= Resolution =
==============

Initially the CHIP-8 had a 64x32px monochrome display.
The ETI 660 had additional 64x48px and 64x64px modes,
and are expected to be supported by modern interpreters.

===========
= Sprites =
===========

CHIP-8 uses a binary-representation of pictures called sprites to paint the display.
Sprites may be up to 15 bytes, for a max size of 8x15px.

Special Sprites
---------------

There are also sprites for the hexadecimal digits (0-F).
These are 5 bytes long, and 8x15px.
They are stored directly in the interpreter's memory.

Example:

+--------+----------+------+
| Sprite | Binary   | Hex  |
+--------+----------+------+
| *****  | 11110000 | 0xF0 |
| *      | 10000000 | 0x80 |
| *****  | 11110000 | 0xF0 |
|     *  | 00010000 | 0x10 |
| *****  | 11110000 | 0xF0 |
+--------+----------+------+

-}

data Resolution
  = Default -- 64x32
  | Tall    -- 64x48
  | Square  -- 64x64
