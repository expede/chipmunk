{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Memory where

import           Chipmunk.Unit
import           ClassyPrelude

{- Memory Layout

A CHIP-8 system has up to 4096 bytes of RAM.
This runs from 0x000 (0) to 0xFFF (4095).

The first 512 bytes (0x000 - 0x1FF) are reserved the interpreter.
As such, most programs begin at 0x200 (512).
Programs intended for the ETI 660 computer started at 0x600 (1536).

-}

type Address = Slab

type Memory = Vector Word8

blank :: Memory
blank = fromList . take 4096 $ repeat 0
