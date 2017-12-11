{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Chipmunk where

import           ClassyPrelude
import           Data.Word

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Memory Layout

A CHIP-8 system has up to 4096 bytes of RAM.
This runs from 0x000 (0) to 0xFFF (4095).

The first 512 bytes (0x000 - 0x1FF) are reserved the interpreter.
As such, most programs begin at 0x200 (512).
Programs intended for the ETI 660 computer started at 0x600 (1536).
-}

newtype Memory = Memory { getMemory :: [Bool] } -- 4096 bytes of RAM

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

data Timers = Timers
  { getDelay :: Word8
  , getSound :: Word8
  }

data Registers = Registers
  { getGeneral :: GeneralRegisters
  , getTimers  :: Timers
  , getI       :: Word16
  , getPC      :: Word16
  , getSP      :: Word8
  }

{- Keyboard

The keyboard is laid out as a 16-key dex keypad:

            +---+---+---+---+
            | 1 | 2 | 3 | C |
            +---+---+---+---+
            | 4 | 5 | 6 | D |
            +---+---+---+---+
            | 7 | 8 | 9 | E |
            +---+---+---+---+
            | A | 0 | B | F |
            +---+---+---+---+

This needs to be emulated for modern keyboards
-}

{- Display

==============
= Resolution =
==============

Initially the CHIP-8 had a 64x32px monochrome display.
The ETI 660 had additional 64x48px and 64x64px modes,
and are expected to be supported by modern interpreters.

===========
= Sprites =
===========

CHIP-8 uses a binary-representation of pictures called sprites to paint the display.
Sprites may be up to 15 bytes, for a max size of 8x15px.

Special Sprites
---------------

There are also sprites for the hexadecimal digits (0-F).
These are 5 bytes long, and 8x15px.
They are stored directly in the interpreter's memory.

Example:

+--------+----------+------+
| Sprite | Binary   | Hex  |
+--------+----------+------+
| *****  | 11110000 | 0xF0 |
| *      | 10000000 | 0x80 |
| *****  | 11110000 | 0xF0 |
|     *  | 00010000 | 0x10 |
| *****  | 11110000 | 0xF0 |
+--------+----------+------+

-}

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

{- Instructions

The original CHIP-8 has 36 instructions.
The Super CHIP-8 adds 10 more (ie: 46).

All instructions are 2-bytes long, and big-endian.

If a sprite is included, it should be padded to aline with RAM.

==========
= Legend =
==========

+-------------+------+-------------------------------+
| Variable    | Bits | Description                   |
+-------------+------+-------------------------------+
| nnn or addr | 12   | Lowest 12 bits                |
| kk or byte  | 8    | Lowest 8 bits                 |
| n or nibble | 4    | Lowest 4 bits                 |
| x           | 4    | Lower 4 bits of the high byte |
| y           | 4    | Upper 4 bits of the low byte  |
+-------------+------+-------------------------------+

                           ---- Nibble
                      --------- Byte
               ---------------- Addr
          0000 0000   0000 0000
High Byte ---------   --------- Low Byte
               ----   ----
                X      Y

================
= Instructions =
================

+------+--------+-----------+--------------------------------------------+
| Hex  | Opcode | Layout    |  Description                               |
+------+--------+-----------+--------------------------------------------+
| 0nnn | SYS    | addr      | IGNORED IN MODERN INTERPRETERS             |
+------+--------+-----------+--------------------------------------------+
| 00E0 | CLS    |           | Clear display                              |
+------+--------+-----------+--------------------------------------------+
| 00EE | RET    |           | Return from subroutine                     |
|      |        |           |                                            |
|      |        |           | 1. Set PC the address on the top of stack  |
|      |        |           | 2. Subtract 1 from stack pointer           |
+------+--------+-----------+--------------------------------------------+
| 1nnn | JP     | addr      | Jump to location nnn. Set PC to nnn.       |
+------+--------+-----------+--------------------------------------------+
| 2nnn | CALL   | addr      | Call suroutine at nnn                      |
|      |        |           |                                            |
|      |        |           | 1. Increment stack pointer                 |
|      |        |           | 2. Put PC at top of stack                  |
|      |        |           | 3. Set PC to nnn                           |
+------+--------+-----------+--------------------------------------------+
| 3xkk | SE     | Vx : byte | Skip next instruction if (Vx == kk)        |
|      |        |           |                                            |
|      |        |           | 1. Compare Vx to kk                        |
|      |        |           | 2. If equal, increment PC by 2             |
+------+--------+-----------+--------------------------------------------+
| 4xkk | SNE    | Vx : byte | Skip next instruction unless (Vx == kk)    |
|      |        |           |                                            |
|      |        |           | 1. Compare Vx to kk                        |
|      |        |           | 2. If inequal, increment PC by 2           |
+------+--------+-----------+--------------------------------------------+
| 5xy0 | SE     | Vx : Vy   | Skip next instruction if (Vx == Vy)        |
|      |        |           |                                            |
|      |        |           | 1. Compare Vx to kk                        |
|      |        |           | 2. If equal, increment PC by 2             |
+------+--------+-----------+--------------------------------------------+
| 6xkk | LD     | Vx : byte | Set Vx to kk                               |
+------+--------+-----------+--------------------------------------------+
| 7xkk | ADD    | Vx : byte | Add kk to the value in Vx, and store in Vx |
+------+--------+-----------+--------------------------------------------+
| 8xy0 | LD     | Vx : Vy   | Set Vx to the value found in Vy            |
+------+--------+-----------+--------------------------------------------+

-}
