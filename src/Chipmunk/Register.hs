{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Register where

import           ClassyPrelude
import           Data.Word

import qualified Chipmunk.Timer as T

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

data GeneralRegisters = GeneralRegisters
  { getV0 :: Word16
  , getV1 :: Word16
  , getV2 :: Word16
  , getV3 :: Word16
  , getV4 :: Word16
  , getV5 :: Word16
  , getV6 :: Word16
  , getV7 :: Word16
  , getV8 :: Word16
  , getV9 :: Word16
  , getVA :: Word16
  , getVB :: Word16
  , getVC :: Word16
  , getVD :: Word16
  , getVE :: Word16
  , getVF :: Word16
  }

data Registers = Registers
  { getGeneral :: GeneralRegisters
  , getTimers  :: T.Timers
  , getI       :: Word16
  , getPC      :: Word16
  , getSP      :: Word8
  }
