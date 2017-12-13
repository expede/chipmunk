{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk where

import           ClassyPrelude

{- Timers & Sound

There are two timers: delay and sound.

==============
= Delay (DT) =
==============

Very simple: holds a number. If nonzero, decrements at the rate of 60Hz.

==============
= Sound (ST) =
==============

The sound timer holds a number.
If it is nonzero, the counter decrements at 60Hz.
A tone is played until the counter is zero.

There is only one tone, and the frequency is not specified in the spec.

-}

data Timers = Timers
  { getDelay :: Word8
  , getSound :: Word8
  }
