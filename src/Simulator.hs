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
  { instructions :: [Word32]
  , controlStoreOutput :: Word32
  }

data AluOutput = AluOutput
  { bits :: Word16
  , n :: Bool
  , z :: Bool
  }

aluOutput :: Word16 -> AluOutput
aluOutput x = AluOutput
  { bits = x
  , n = x < 0
  , z = x == 0
  }

tick :: Micro16State -> Micro16State
tick s@Micro16State {clock} = doTick newClock s {clock = newClock}
  where
    newClock = nextPhase clock
    doTick :: Clock -> Micro16State -> Micro16State
    doTick = \case
      Phase1 -> tickRegisters . tickABusDecoder . tickBBusDecoder . tickMir
      Phase2 -> tickMicroSeqLogic . tickShifter . tickAlu . tickAMux . tickABus . tickBBus
      Phase3 -> tickMar . tickMbr
      Phase4 -> tickControlStore . tickMic . tickSBus . tickSBusDecoder

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
tickBBus :: Micro16State -> Micro16State
tickBBus s@Micro16State {registers} = s {bBus = toBBus registers}

tickABus :: Micro16State -> Micro16State
tickABus s@Micro16State {registers} = s {aBus = toABus registers}

tickAMux :: Micro16State -> Micro16State
tickAMux s@Micro16State {aBus, mar, mir} = s {aMux = newValue}
  where
    newValue = if testBit mir 31
      then mar
      else aBus

tickAlu :: Micro16State -> Micro16State
tickAlu s@Micro16State {aMux, bBus, mir} = s {alu = newOutput}
  where
    newOutput = aluOutput $ value $ (mir `shiftR` 27) .&. 3
    value = \case
      0 -> aMux
      1 -> aMux + bBus
      2 -> aMux .&. bBus
      3 -> complement aMux

tickShifter :: Micro16State -> Micro16State
tickShifter s@Micro16State {alu, mir} = s {shifter = newOutput}
  where
    newOutput = value $ (mir `shiftR` 25) .&. 3
    b = bits alu
    value = \case
      0 -> b
      1 -> b `shiftL` 1
      2 -> b `shiftR` 1

tickMicroSeqLogic :: Micro16State -> Micro16State
tickMicroSeqLogic s@Micro16State {alu, mir} = s {microSeqLogic = newOutput}
  where
    newOutput = value $ (mir `shiftR` 29) .&. 3
    value = \case
      0 -> False
      1 -> n alu
      2 -> z alu
      3 -> True

--
-- Phase 3
--

tickMbr :: Micro16State -> Micro16State
tickMbr s@Micro16State {shifter, mir, mbr} = s {mbr = newOutput}
  where
    newOutput = if testBit mir 24
      then shifter
      else mbr

tickMar :: Micro16State -> Micro16State
tickMar s@Micro16State {shifter, mir, mar} = s {mar = newOutput}
  where
    newOutput = if testBit mir 23
      then shifter
      else mar

--
-- Phase 4
--

tickSBusDecoder :: Micro16State -> Micro16State
tickSBusDecoder s@Micro16State{mir} = s {sBusDecoder = writableRegFromMir (regAtOffset 16 mir)}

tickSBus :: Micro16State -> Micro16State
tickSBus s@Micro16State{sBusDecoder, registers, shifter} = s {registers = registers {state = newState}}
  where
    newState = Map.insert sBusDecoder shifter (state registers)

tickMic :: Micro16State -> Micro16State
tickMic s@Micro16State{microSeqLogic, mic, mir} = s {mic = newCounter}
  where
    newCounter = if microSeqLogic
      then fromInteger $ toInteger (mir .&. 255)
      else mic + 1


tickControlStore :: Micro16State -> Micro16State
tickControlStore s@Micro16State{mic, controlStore} = s {controlStore = newControlStore}
  where
    newControlStore = controlStore {controlStoreOutput = instructions controlStore !! fromIntegral mic}
