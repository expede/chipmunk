{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Register where

import           Chipmunk.Timer
import           Chipmunk.Unit
import           ClassyPrelude

{- Registers

===========
= General =
===========

The CHIP-8 has 16 x 8-bit registers, generally named V0-VF.
VF is not typically used by programs, since it is used as a flag for some
instructions (ie: it's used by the interpreter itself).

=========
= Stack =
=========

A 16-vector of 16-bit values.
Used to store addresses that the interpreter should return to
when completing subroutines.

=====================
= Special Registers =
=====================

The I Register
--------------

I is a special 16-bit register, typically used to store addresses.
As such, often only the first 12 bits are used.

Timer & Sound
-------------

The timer and sound registers are each 8-bits.
When they are nonzero, they automatically decrement at 60Hz.

====================
= Pseudo-Registers =
====================

These are internal to the interpreter

The 16-bit Program Counter (PC) holds the currently executing address.

The 8-bit Stack Pointer (SP) points to the top of the stack.
-}

type ID = Nibble

data Registers = Registers
  { getV0 :: Doublet
  , getV1 :: Doublet
  , getV2 :: Doublet
  , getV3 :: Doublet
  , getV4 :: Doublet
  , getV5 :: Doublet
  , getV6 :: Doublet
  , getV7 :: Doublet
  , getV8 :: Doublet
  , getV9 :: Doublet
  , getVA :: Doublet
  , getVB :: Doublet
  , getVC :: Doublet
  , getVD :: Doublet
  , getVE :: Doublet
  , getVF :: Doublet
  , getI  :: Doublet
  }
  deriving (Eq, Show)

lookup :: Nibble -> (Registers -> Doublet)
lookup registerID =
  case getNibble registerID of
    0  -> getV0
    1  -> getV1
    2  -> getV2
    3  -> getV3
    4  -> getV4
    5  -> getV5
    6  -> getV6
    7  -> getV7
    8  -> getV8
    9  -> getV9
    10 -> getVA
    11 -> getVB
    12 -> getVC
    13 -> getVD
    14 -> getVE
    15 -> getVF
    e  -> error $ "No such register: " ++ show e
