{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Memory where

import           ClassyPrelude
import           Data.Word

{- Memory Layout

A CHIP-8 system has up to 4096 bytes of RAM.
This runs from 0x000 (0) to 0xFFF (4095).

The first 512 bytes (0x000 - 0x1FF) are reserved the interpreter.
As such, most programs begin at 0x200 (512).
Programs intended for the ETI 660 computer started at 0x600 (1536).
-}

newtype Memory = Memory { getMemory :: [Bool] } -- 4096 bytes of RAM
