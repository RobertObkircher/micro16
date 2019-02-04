module Simulator where

import qualified Compiler as C
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
  , mic :: Word8
  , aBus :: Word16
  , bBus :: Word16
  , aMux :: Word16
  , mir :: Word32
  , microSeqLogic :: Bool
  , alu :: AluOutput
  , shifter :: Word16
  , mbr :: Word16 -- ^ TODO
  , mar :: Word16
  }

data Clock = Phase1 | Phase2 | Phase3 | Phase4

data Registers = Registers
  { toABus :: Word16 -- ^ 16 bits used
  , toBBus :: Word16 -- ^ 16 bits used
  , state :: Map C.WritableReg Word16
  }

data AluOutput = AluOutput
  { bits :: Word16
  , n :: Bool
  , z :: Bool
  }

