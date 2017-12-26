{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Stack where

import           Chipmunk.Unit
import           ClassyPrelude

-- The stack is an array of 16 16-bit values
-- Chip-8 allows for up to 16 levels of nested subroutines.

type Stack = Vector Byte

data StackMachine = StackMachine
  { getStack   :: Stack
  , getPointer :: Byte
  }

push :: Stack -> Byte -> Stack
push stack newByte =

-- There are also some "pseudo-registers" which are not accessable from Chip-8 programs. The program counter (PC) should be 16-bit, and is used to store the currently executing address. The stack pointer (SP) can be 8-bit, it is used to point to the topmost level of the stack.
-- , getProgramCounter :: Doublet
