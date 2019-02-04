{-# LANGUAGE  LambdaCase #-}
{-# LANGUAGE  NamedFieldPuns #-}

module Simulator where

import qualified Compiler as C
import           Data.Bits
import Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word

-- Stores the current outputs for each component
data Micro16State = Micro16State
  { sBusDecoder :: C.WritableReg
  , bBusDecoder :: C.Reg
  , aBusDecoder :: C.Reg
  , clock :: Clock
  , registers :: Registers
  , aBus :: Word16
  , bBus :: Word16
  , aMux :: Word16
  , mic :: Word8
  , controlStore :: ControlStore
  , mir :: Word32
  , microSeqLogic :: Bool
  , alu :: AluOutput
  , shifter :: Word16
  , mbr :: Word16 -- ^ TODO
  , mar :: Word16
  }

data Clock = Phase1 | Phase2 | Phase3 | Phase4

nextPhase :: Clock -> Clock
nextPhase = \case
  Phase1 -> Phase2
  Phase2 -> Phase3
  Phase3 -> Phase4
  Phase4 -> Phase1

updateClock :: Micro16State -> Micro16State
updateClock s@Micro16State{clock} = s {clock = nextPhase clock}

data Registers = Registers
  { toABus :: Word16 -- ^ 16 bits used
  , toBBus :: Word16 -- ^ 16 bits used
  , state :: Map C.WritableReg Word16
  }

data ControlStore = ControlStore
  { instructions :: [Int]
  , controlStoreOutput :: Word32
  }

data AluOutput = AluOutput
  { bits :: Word16
  , n :: Bool
  , z :: Bool
  }

tick :: Micro16State -> Micro16State
tick s@Micro16State {clock} = doTick newClock s {clock = newClock}
  where
    newClock = nextPhase clock
    doTick :: Clock -> Micro16State -> Micro16State
    doTick = \case
      Phase1 -> tickRegisters . tickABusDecoder . tickBBusDecoder . tickMir
      Phase2 -> id
      Phase3 -> id
      Phase4 -> id

--
-- Phase 1
--

tickMir :: Micro16State -> Micro16State
tickMir s@Micro16State{controlStore} = s { mir = controlStoreOutput controlStore}

regFromMir :: Word32 -> C.Reg
regFromMir = \case
  0 -> C.Zero
  1 -> C.One
  2 -> C.MinusOne
  n -> C.WR $ writableRegFromMir n

writableRegFromMir :: Word32 -> C.WritableReg
writableRegFromMir = \case
  3 -> C.PC
  4 -> C.R0
  5 -> C.R1
  6 -> C.R2
  7 -> C.R3
  8 -> C.R4
  9 -> C.R5
  10 -> C.R6
  11 -> C.R7
  12 -> C.R8
  13 -> C.R9
  14 -> C.R10
  15 -> C.AC

regAtOffset :: Int -> Word32 -> Word32
regAtOffset i mic = (mic `shiftR` i) .&. 15

tickBBusDecoder :: Micro16State -> Micro16State
tickBBusDecoder s@Micro16State{mir} = s {bBusDecoder = regFromMir (regAtOffset 12 mir)}

tickABusDecoder :: Micro16State -> Micro16State
tickABusDecoder s@Micro16State{mir} = s {aBusDecoder = regFromMir (regAtOffset 8 mir)}

tickRegisters :: Micro16State -> Micro16State
tickRegisters s@Micro16State{aBusDecoder, bBusDecoder, registers} = s
  { registers = registers
    { toABus = lookup aBusDecoder
    , toBBus = lookup bBusDecoder
    }
  }
  where
    lookup :: C.Reg -> Word16
    lookup = \case
      C.Zero -> 0
      C.One -> 1
      C.MinusOne -> -1
      C.WR wr -> Map.findWithDefault 0 wr (state registers)

--
-- Phase 2
--
