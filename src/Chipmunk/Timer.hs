{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Timer where

import           Chipmunk.Unit
import           ClassyPrelude
import           System.Clock

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

class MonadIO m => Time m where
  getNow :: m UTCTime

data Timers = Timers
  { getDelay    :: Byte
  , getSound    :: Byte
  , getLastTick :: !Nanoseconds
  }
  deriving (Eq, Show)

type Nanoseconds = Integer

run :: Time m => Timers -> m Timers
run timers@(Timers {getLastTick = previousTick}) = do
  now <- liftIO $ getTime Realtime

  let
    nowStamp = toNanoSecs now
    duration = nowStamp - previousTick

  return $
    if duration >= refreshRate
       then tickAll timers nowStamp
       else timers

tickAll :: Timers -> Nanoseconds -> Timers
tickAll (Timers {getDelay, getSound}) timestamp =
  Timers { getDelay    = tick getDelay
         , getSound    = tick getSound
         , getLastTick = timestamp
         }

tick :: Byte -> Byte
tick 0   = 0
tick num = num - 1

-- ~60Hz
refreshRate :: Nanoseconds
refreshRate = 16666667
