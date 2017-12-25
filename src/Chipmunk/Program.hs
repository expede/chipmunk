{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Program where

import           Chipmunk.Unit
import           ClassyPrelude

type Program = Vector Doublet

toProgram :: [Doublet] -> Program
toProgram = fromList

paddedPairs :: Num a => [a] -> [(a, a)]
paddedPairs []           = []
paddedPairs (x : [])     = [(x, 0)]
paddedPairs (x : y : zs) = (x, y) : paddedPairs zs

fetchProgram :: MonadIO m => FilePath -> m [Doublet]
fetchProgram path = do
  program <- readFile path

  let
    bytePairs = paddedPairs $ unpack program
    doublets  = (\(a, b) -> merge a b) <$> bytePairs

  return doublets
