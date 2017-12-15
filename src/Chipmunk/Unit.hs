{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Unit
  ( Nibble ()
  , Byte   ()
  , Slab   ()
  , toNibble
  , getNibble
  , toByte
  , getByte
  , toSlab
  , getSlab
  , getLowSignifcantNibble
  , getHighInsignifcantNibble
  ) where

import           ClassyPrelude
import           Data.Bits
import           Data.Word

{- Units

+--------------------+------+-------------------------------+
| Variable           | Bits | Description                   |
+--------------------+------+-------------------------------+
| nnn, addr, or slab | 12   | Lowest 12 bits                |
| kk or byte         | 8    | Lowest 8 bits                 |
| n or nibble        | 4    | Lowest 4 bits                 |
| x                  | 4    | Lower 4 bits of the high byte |
| y                  | 4    | Upper 4 bits of the low byte  |
+--------------------+------+-------------------------------+

               ┌----------------┐ Slab
               |      ┌---------┤ Byte
               |      |    ┌----┤ Nibble
           0000 0000   0000 0000  Doublet
          |   x└----┤ ├----┘ y  |
          |         | |         |
High Byte └---------┘ └---------┘ Low Byte

-}

newtype Nibble =
  Nibble { getNibble :: Word8  } -- 0000
  deriving (Show, Eq)

newtype Byte =
  Byte { getByte   :: Word8  }   -- 0000 0000
  deriving (Show, Eq)

newtype Slab =
  Slab { getSlab   :: Word16 }   -- 0000 0000 0000
  deriving (Show, Eq)

toNibble :: Word8 -> Nibble
toNibble byte = Nibble $ byte `rem` 16

toByte :: Word16 -> Byte
toByte doublet = Byte . fromIntegral $ doublet `rem` 256

toSlab :: Word16 -> Slab
toSlab doublet = Slab $ doublet `rem` 4096

getLowSignifcantNibble :: Word16 -> Nibble
getLowSignifcantNibble doublet = toNibble . fromIntegral $ doublet `shift` (-8)

getHighInsignifcantNibble :: Word16 -> Nibble
getHighInsignifcantNibble doublet = toNibble . fromIntegral $ doublet `shift` (-4)
