{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Unit
  ( Nibble  ()
  , Byte
  , Slab    ()
  , Doublet
  , toNibble
  , getNibble
  , toByte
  , toSlab
  , merge
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

newtype Nibble = Nibble { getNibble :: Word8 }
  deriving (Eq, Ord, Show)

type Byte =  Word8

newtype Slab = Slab { getSlab :: Word16 }
  deriving (Eq, Ord, Show)

type Doublet = Word16

toNibble :: Byte -> Nibble
toNibble byte = Nibble $ byte `rem` 16

toByte :: Doublet -> Byte
toByte = fromIntegral

toSlab :: Doublet -> Slab
toSlab doublet = Slab $ doublet `rem` 4096

merge :: Byte -> Byte -> Doublet
merge x y = (shift (fromIntegral x :: Doublet) 8) + (fromIntegral y :: Doublet)

getLowSignifcantNibble :: Doublet -> Nibble
getLowSignifcantNibble doublet = toNibble . fromIntegral $ doublet `shift` (-8)

getHighInsignifcantNibble :: Doublet -> Nibble
getHighInsignifcantNibble doublet = toNibble . fromIntegral $ doublet `shift` (-4)
