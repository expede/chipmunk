{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Memory where

import           Chipmunk.Program
import           Chipmunk.Unit
import           ClassyPrelude
import           Data.Vector      (update_)

{- Memory Layout

A CHIP-8 system has up to 4096 bytes of RAM.
This runs from 0x000 (0) to 0xFFF (4095).

The first 512 bytes (0x000 - 0x1FF) are reserved the interpreter.
As such, most programs begin at 0x200 (512).
Programs intended for the ETI 660 computer started at 0x600 (1536).

-}

type Address = Slab

type Memory = Vector Doublet

totalSpace :: Int
totalSpace = 4096

blank :: Memory
blank = replicate totalSpace 0

startIndex :: Int
startIndex = 512

startAddress :: Address
startAddress = toSlab 512

mergeProgram :: Memory -> Program -> Memory
mergeProgram memory program =
  update_ memory indexVect program

  where
    indexVect = fromList $ take (length program) [startIndex..]

loadProgram :: MonadIO m => Memory -> FilePath -> m Memory
loadProgram memory path = do
  program <- toProgram <$> fetchProgram path
  return $ mergeProgram memory program
