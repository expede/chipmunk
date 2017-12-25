{-# LANGUAGE NoImplicitPrelude #-}

module Chipmunk.Instruction where

import qualified Chipmunk.Memory as Memory
import           Chipmunk.Unit
import           ClassyPrelude

{- Instructions

The original CHIP-8 has 36 instructions.
The Super CHIP-8 adds 10 more (ie: 46).

All instructions are 2-bytes long, and big-endian.

If a sprite is included, it should be padded to aline with RAM.

================
= Instructions =
================

+------+--------+------------------+--------------------------------------------+
| Hex  | Opcode | ne at nnn                      |
|      |        |                  |                                            |
|      |        |                  | 1. Increment stack pointer                 |
|      |        |                  | 2. Put PC at top of stack                  |
|      |        |                  | 3. Set PC to nnn                           |
+------+--------+------------------+--------------------------------------------+
| 3xkk | SE     | Vx : byte        | Skip next instruction if (Vx == kk)        |
|      |        |                  |                                            |
|      |        |                  | 1. Compare Vx to kk                        |
|      |        |                  | 2. If equal, increment PC by 2             |
+------+--------+------------------+--------------------------------------------+
| 4xkk | SNE    | Vx : byte        | Skip next instruction unless (Vx == kk)    |
|      |        |                  |                                            |
|      |        |                  | 1. Compare Vx to kk                        |
|      |        |                  | 2. If inequal, increment PC by 2           |
+------+--------+------------------+--------------------------------------------+
| 5xy0 | SE     | Vx : Vy          | Skip next instruction if (Vx == Vy)        |
|      |        |                  |                                            |
|      |        |                  | 1. Compare Vx to kk                        |
|      |        |                  | 2. If equal, increment PC by 2             |
+------+--------+------------------+--------------------------------------------+
| 6xkk | LD     | Vx : byte        | Set Vx to kk                               |
+------+--------+------------------+--------------------------------------------+
| 7xkk | ADD    | Vx : byte        | Add kk to the value in Vx, and store in Vx |
+------+--------+------------------+--------------------------------------------+
| 8xy0 | LD     | Vx : Vy          | Set Vx to the value found in Vy            |
+------+--------+------------------+--------------------------------------------+
| 8xy1 | OR     | Vx : Vy          | Set Vx to (Vx OR Vy)                       |
|      |        |                  |                                            |
|      |        |                  | 1. Perform a bitwise OR of Vx and Vy       |
|      |        |                  | 2. Set Vx to the result                    |
+------+--------+------------------+--------------------------------------------+
| 8xy2 | AND    | Vx : Vy          | Set Vx to (Vx AND Vy)                      |
|      |        |                  |                                            |
|      |        |                  | 1. Perform a bitwise AND of Vx and Vy      |
|      |        |                  | 2. Set Vx to the result                    |
+------+--------+------------------+--------------------------------------------+
| 8xy3 | XOR    | Vx : Vy          | Set Vx to (Vx XOR Vy)                      |
|      |        |                  |                                            |
|      |        |                  | 1. Perform a bitwise XOR of Vx and Vy      |
|      |        |                  | 2. Set Vx to the result                    |
+------+--------+------------------+--------------------------------------------+
| 8xy4 | ADD    | Vx : Vy          | Set Vx to (Vx + Vy), and set VF to carry   |
|      |        |                  |                                            |
|      |        |                  | 1. Add Vx and Vy                           |
|      |        |                  | 2. If (result > 255), set VF to 1, else 0  |
|      |        |                  | 3. Store the lowest 8 bits in Vx           |
+------+--------+------------------+--------------------------------------------+
| 8xy5 | SUB    | Vx : Vy          | Set Vx to (Vx - Vy), and set VF to borrow  |
|      |        |                  |                                            |
|      |        |                  | 1. If (Vx > Vy), set VF to 1, else 0       |
|      |        |                  | 2. Subtract Vy from Vx                     |
|      |        |                  | 3. Store the lowest 8 bits in Vx           |
+------+--------+------------------+--------------------------------------------+
| 8xy6 | SHR    | Vx : Vy          | Divide Vx by 2, store any dividend in VF   |
|      |        |                  | (ie: shift right)                          |
|      |        |                  |                                            |
|      |        |                  | 1. If Vx's lowest bit is 1,                |
|      |        |                  |      set VF to 1,                          |
|      |        |                  |      otherwise set it to 0                 |
|      |        |                  |                                            |
|      |        |                  | 2. Set Vx to (Vx / 2)                      |
+------+--------+------------------+--------------------------------------------+
| 8xy7 | SUBN   | Vx : Vy          | Set Vx to (Vy - Vx), and set VF to borrow  |
|      |        |                  |                                            |
|      |        |                  | 1. If (Vy > Vx), set VF to 1, else 0       |
|      |        |                  | 2. Subtract Vx from Vy                     |
|      |        |                  | 3. Store the lowest 8 bits in Vx           |
+------+--------+------------------+--------------------------------------------+
| 8xyE | SHL    | Vx : Vy          | Double, and store any rounding in VF       |
|      |        |                  | (ie: shift left)                           |
|      |        |                  |                                            |
|      |        |                  | 1. If Vx's highest bit is 1,               |
|      |        |                  |      set VF to 1,                          |
|      |        |                  |      otherwise set it to 0                 |
|      |        |                  |                                            |
|      |        |                  | 2. Set Vx to (Vx * 2)                      |
+------+--------+------------------+--------------------------------------------+
| 9xy0 | SNE    | Vx : Vy          | Skip next instruction if (Vx /= Vy)        |
|      |        |                  |                                            |
|      |        |                  | 1. Compare Vx and Vy                       |
|      |        |                  | 2. If they're different,                   |
|      |        |                  |      increase program counter by 2         |
+------+--------+------------------+--------------------------------------------+
| Annn | LD     | I : nnn          | Set I to nnn                               |
+------+--------+------------------+--------------------------------------------+
| Bnnn | JP     | V0 : nnn         | Jump to location (nnn + V0)                |
+------+--------+------------------+--------------------------------------------+
| Cxkk | RND    | Vx : byte        | Set Vx to (random byte AND kkk)            |
+------+--------+------------------+--------------------------------------------+
| Dxyn | DRW    | Vx : Vy : nibble | Draw n-byte sprite starting at address I   |
|      |        |                  |   at coords (Vx, Vy),                      |
|      |        |                  |   and set VD to collision                  |
|      |        |                  |                                            |
|      |        |                  | 1. Look up the address found in I          |
|      |        |                  | 2. Read n bytes starting at that address   |
|      |        |                  | 3. Draw the bytes at coordinates (Vx, Vy)  |
|      |        |                  | 4. Sprites are XORed with                  |
|      |        |                  |      the existing screen state             |
|      |        |                  | 5. If any pixels are erased,               |
|      |        |                  |      set VF to 1,                          |
|      |        |                  |      else 0                                |
|      |        |                  | 6. Attempting to draw out of bounds        |
|      |        |                  |      wraps around to the other side        |
|      |        |                  |      of the screen PAC-MAN style           |
+------+--------+------------------+--------------------------------------------+
| Ex9E | SKP    | Vx               | Skip next instruction if key Vx is pressed |
|      |        |                  |                                            |
|      |        |                  | 1. Get the value in Vx                     |
|      |        |                  | 2. Read the keyboard                       |
|      |        |                  | 3. If the key with Vx's value is           |
|      |        |                  |      currently pressed, increase the       |
|      |        |                  |      program counter (PC) by 2             |
+------+--------+------------------+--------------------------------------------+
| ExA1 | SKNP   | Vx               | Skip next instruction if key Vx            |
|      |        |                  |   is not pressed                           |
|      |        |                  |                                            |
|      |        |                  | 1. Get the value in Vx                     |
|      |        |                  | 2. Read the keyboard                       |
|      |        |                  | 3. If the key with Vx's value is           |
|      |        |                  |      currently unpressed, increase the     |
|      |        |                  |      program counter (PC) by 2             |
+------+--------+------------------+--------------------------------------------+
| Fx07 | LD     | Vx : DT          | Set Vx to the value in the delay timer     |
+------+--------+------------------+--------------------------------------------+
| Fx0A | LD     | Vx : K           | Wait for key input, and store value in Vx  |
|      |        |                  |                                            |
|      |        |                  | Stop all execution while waiting           |
+------+--------+------------------+--------------------------------------------+
| Fx15 | LD     | DT : Vx          | Set the delay timer to the value in Vx     |
+------+--------+------------------+--------------------------------------------+
| Fx18 | LD     | ST : Vx          | Set the sound timer to the value in Vx     |
+------+--------+------------------+--------------------------------------------+
| Fx1E | ADD    | I : Vx           | Set I to (I + Vx)                          |
+------+--------+------------------+--------------------------------------------+
| Fx29 | LD     | F : Vx           | Set I to the low 4 bits of sprite in Vx    |
+------+--------+------------------+--------------------------------------------+
| Fx33 | LD     | B : Vx           | Store the decimal Vx in I through (I + 2)  |
|      |        |                  |                                            |
|      |        |                  | 1. Take the decimal value of Vx            |
|      |        |                  | 2. Set I to the 100s digit                 |
|      |        |                  | 3. Set I + 1 to the 10s digit              |
|      |        |                  | 4. Set I + 2 to the 1s digit               |
+------+--------+------------------+--------------------------------------------+
| Fx55 | LD     | [I] : Vx         | Store V0 through Vx at locations           |
|      |        |                  |   starting at I                            |
+------+--------+------------------+--------------------------------------------+
| Fx65 | LD     | Vx : [I]         | Read out values starting at I              |
|      |        |                  |   and store in V0 through Vx               |
+------+--------+------------------+--------------------------------------------+

==============================
= Super Chip-48 Instructions =
==============================

00Cn - SCD nibble
00FB - SCR
00FC - SCL
00FD - EXIT
00FE - LOW
00FF - HIGH
Dxy0 - DRW Vx, Vy, 0
Fx30 - LD HF, Vx
Fx75 - LD R, Vx
Fx85 - LD Vx, R

-}

type RegisterID = Nibble

data Instruction
  = Ignore                                                         -- 0x0NNN
  | ClearDisplay                                                   -- 0x00E0
  | ReturnFromSubroutine                                           -- 0x00EE

  | JumpTo                    Memory.Address                       -- 0x1NNN
  | CallSubroutineAt          Memory.Address                       -- 0x2NNN

  | SkipIfRegisterEqualVal    RegisterID     Byte                  -- 0x3XKK
  | SkipIfRegisterNotEqualVal RegisterID     Byte                  -- 0x4XKK
  | SkipIfRegisterValsEqual   RegisterID     RegisterID            -- 0x5XY0
  | SetRegisterTo             RegisterID     Byte                  -- 0x6XKK
  | AddValToRegister          RegisterID     Byte                  -- 0x7XKK

  | ReplaceVxWithVy           RegisterID     RegisterID            -- 0x8XY0
  | VxOrVy                    RegisterID     RegisterID            -- 0x8XY1
  | VxAndVy                   RegisterID     RegisterID            -- 0x8XY2
  | VxXorVy                   RegisterID     RegisterID            -- 0x8XY3
  | AddVxVy                   RegisterID     RegisterID            -- 0x8XY4
  | SubtractVxFromVy          RegisterID     RegisterID            -- 0x8XY5
  | SetVxToRightShiftVy       RegisterID     RegisterID            -- 0x8XY6
  | SubtractVyFromVx          RegisterID     RegisterID            -- 0x8XY7
  | SetVxToLeftShiftVy        RegisterID     RegisterID            -- 0x8XYE

  | SkipIfVxNotEqualToVy      RegisterID     RegisterID            -- 0x9XY0

  | SetIToAddressVal          Memory.Address                       -- 0xANNN
  | JumpToAddressPlusV0       Memory.Address                       -- 0xBNNN

  | SetVxToRandomWithMask     RegisterID     Byte                  -- 0xCXKK
  | DrawSpriteNAtCoordsVxVy   RegisterID     RegisterID Nibble     -- 0xDXYN

  | SkipIfVxKeyDown           RegisterID                           -- 0xEX9E
  | SkipIfVxKeyUp             RegisterID                           -- 0xEXA1

  | SetVxToDelayTimer         RegisterID                           -- 0xFX07
  | WaitForInputAndStoreInVx  RegisterID                           -- 0xFX0A
  | SetDelayTimerToVx         RegisterID                           -- 0xFX15
  | SetSoundTimerToVx         RegisterID                           -- 0xFX18
  | AddVxToI                  RegisterID                           -- 0xFX1E
  | SetIToSpriteNibbleAtVx    RegisterID                           -- 0xFX29
  | StoreVxDecimalAtI         RegisterID                           -- 0xFX33
  | CopyV0ToVxAtI             RegisterID                           -- 0xFX55
  | CopyFromIIntoV0ToVx       RegisterID                           -- 0xFX65
  deriving (Eq, Show)

toInstruction :: Doublet -> Maybe Instruction
toInstruction raw =
  case getNibble $ highestNibble raw of
    0  -> case raw of
           224 -> Just ClearDisplay
           238 -> Just ReturnFromSubroutine
           _   -> Just Ignore

    1  -> nnn' JumpTo
    2  -> nnn' CallSubroutineAt

    3  -> xkk' SkipIfRegisterEqualVal
    4  -> xkk' SkipIfRegisterNotEqualVal

    5  -> case getNibble . toNibble $ fromIntegral raw of
           0 -> xy' SkipIfRegisterValsEqual
           _ -> Nothing

    6  -> xkk' SetRegisterTo
    7  -> xkk' AddValToRegister

    8  -> case getNibble $ lowestNibble raw of
           0  -> xy' ReplaceVxWithVy

           1  -> xy' VxOrVy
           2  -> xy' VxAndVy
           3  -> xy' VxXorVy

           4  -> xy' AddVxVy
           5  -> xy' SubtractVxFromVy
           6  -> xy' SetVxToRightShiftVy
           7  -> xy' SubtractVyFromVx
           14 -> xy' SetVxToLeftShiftVy

           _  -> Nothing

    9  -> xy' SkipIfVxNotEqualToVy

    10 -> nnn' SetIToAddressVal
    11 -> nnn' JumpToAddressPlusV0
    12 -> xkk' SetVxToRandomWithMask

    13 -> xyn' DrawSpriteNAtCoordsVxVy

    14 -> case toByte raw of
           158 -> x' SkipIfVxKeyDown
           161 -> x' SkipIfVxKeyUp
           _   -> Nothing

    15 -> case toByte raw of
           7   -> x' SetDelayTimerToVx
           10  -> x' WaitForInputAndStoreInVx
           21  -> x' SetDelayTimerToVx
           24  -> x' SetSoundTimerToVx
           30  -> x' AddVxToI
           41  -> x' SetIToSpriteNibbleAtVx
           51  -> x' StoreVxDecimalAtI
           85  -> x' CopyV0ToVxAtI
           101 -> x' CopyFromIIntoV0ToVx
           _   -> Nothing

    _  -> Nothing

    where
      x'   = Just . x   raw
      xy'  = Just . xy  raw
      xyn' = Just . xyn raw
      xkk' = Just . xkk raw
      nnn' = Just . nnn raw

x :: Doublet -> (Nibble -> Instruction) -> Instruction
x num constructor = constructor (highInsignifcantNibble num)

xy :: Doublet -> (Nibble -> Nibble -> Instruction) -> Instruction
xy num constructor = constructor (lowSignifcantNibble num) (highInsignifcantNibble num)

xyn :: Doublet -> (Nibble -> Nibble -> Nibble -> Instruction) -> Instruction
xyn num constructor =
  constructor
    (lowSignifcantNibble num)
    (highInsignifcantNibble num)
    (lowestNibble num)

xkk :: Doublet -> (Nibble -> Byte -> Instruction) -> Instruction
xkk num constructor = constructor (lowSignifcantNibble num) (toByte num)

nnn :: Doublet -> (Slab -> Instruction) -> Instruction
nnn num constructor = constructor $ toSlab num
