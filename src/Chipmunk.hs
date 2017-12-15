{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Chipmunk where

import           ClassyPrelude
import           Data.Word

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Machine Design

┌---------------------------------------------------------┐
|                                                         |
|    V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 VA VB VC VD VE VF   I  |
|     ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑  ↑   ↑  |
|     └--┴--┴--┴--┴--┴--┴--┴-┬┴--┴--┴--┴--┴--┴--┴--┘   |  |
|                            |   ┌---------------------┘  |
|                            ↓   ↓                        |
|                         ┌---------┐                     |
| ┌---------------┐       |         |                     |
| | Stack Pointer | ←---- |         | ←-------------------┼--- Keyboard
| |      |        |       |         |                     |
| |      └→ Stack | ←---→ |         | --→ Frame Buffer ---┼--→ Hardware Display
| └---------------┘       |   CPU   |                     |
|                         |         |                     |
|       Delay Timer ←---→ |         | --→ Sound Timer ----┼--→ 1-bit Sound Channel
|                         |         |                     |
|   Program Counter ←---→ |         |                     |
|          |              |         |                     |
|          |              └---------┘                     |
|          |                 ↑    ↑                       |
|          |                 |    |                       |
|          |                 ↓    |                       |
|          |     ┌--------------┬--------------------┐    |
|          └---→ | Memory (4kB) | Font Sprites (80B) |    |
|                └--------------┴--------------------┘    |
|                                                         |
└---------------------------------------------------------┘

-}
