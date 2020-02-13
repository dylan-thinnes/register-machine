{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Register.Macro where

import Register

import qualified Data.Map as M
import Data.Map ((!?))
import Data.Function ((&))

import Data.Functor.Classes
import Data.Functor.Identity

import Text.ParserCombinators.ReadP

import Data.Default

import qualified Control.Lens as L
import Control.Lens ((^.), (.~), (%~))

import Data.Bitraversable
import Data.Bifunctor

-- Add the MacroData instruction, and sum it with any instruction using Macro
type Macro instr = InstrSum MacroData instr
pattern YMacro x <- InstrL x where
    YMacro x = InstrL x
pattern NMacro x <- InstrR x where
    NMacro x = InstrR x

data MacroData label
    = MacroData
    { name :: String
    , macroRegisters :: [Register]
    , macroLabels :: [label]
    }
    deriving (Eq, Ord, Show, Functor)

-- Submachines encapsulate the concept of running a register machine inside of
-- a single register machine
-- They are useful for defining macros
data Submachine instr label
    = Submachine 
    { -- The GCode defining the machine
      gcode :: GCode instr label
      -- Registers to take as inputs from the current machine state, mapped to R0, R1, etc.
    , inputRegs :: M.Map Register Register
      -- Registers to send as outputs (key is register in macro to be read
      -- from, value is register in macro to be sent to)
    , resultRegs :: M.Map Register Register
    }

instance (Functor instr) => Functor (Submachine instr) where
    fmap f (Submachine {..})
        = Submachine
            { resultRegs, inputRegs
            , gcode = fmap (fmap (bimap f id)) gcode
            }

-- Resolve the macros in an assembly to Submachines using a natural
-- transformation
-- This transformation is composed with an arbitrary monad (e.g. IO)
resolveMacros :: (Functor i1, Functor i2, Monad m)
                 -- The natural transformation between MacroData and
                 -- Submachine, with no awareness of labels
              => (forall label. MacroData label -> m (Submachine i1 label))
                 -- The final transformation from Macro to InstrSum and Submachine
              -> GAssembly (Macro i2) label
              -> m (GAssembly (InstrSum (Submachine i1) i2) label)
resolveMacros converter = convertInstrs $ injLNatTrans converter

-- Resolve the macros with a pure natural transformation
resolveMacrosPure :: (Functor i1, Functor i2)
                     -- The natural transformation between MacroData and
                     -- Submachine, with no awareness of labels
                  => (forall label. MacroData label -> Submachine i1 label)
                     -- The final transformation from Macro to InstrSum and Submachine
                  -> GAssembly (Macro i2) label
                  -> GAssembly (InstrSum (Submachine i1) i2) label
resolveMacrosPure converter = runIdentity . resolveMacros (Identity . converter)

-- We also define further that Submachines are instructions too
instance (Instruction instr values, Default values) => Instruction (Submachine instr) values where
    interpret (Submachine {..}) machinestate
      = machinestate
              -- Overwrite registers in machine that were set in the macro
            & L.over registers (M.union finalRegisters)
              -- Increment the current position
            & L.over position changePosition
        where
            -- Useful for register mapping in submachine macros
            restrictWithMap :: Ord k => M.Map k k -> M.Map k a -> M.Map k a
            restrictWithMap keysMapping
              = M.mapMaybeWithKey (\k a -> const a <$> M.lookup k keysMapping)

            -- Define the initial state
            initialState = GMState (restrictWithMap inputRegs $ _registers machinestate)
                                   (Right $ Position 0)

            -- Run the machine to its final state, query the registers and the
            -- final position
            finalMachine = runEnd $ GM gcode initialState
            finalRegisters = finalMachine
                           & _state & _registers
                           & restrictWithMap resultRegs

            -- If the final position is Left, assume that was a jump for a
            -- label to the external machine
            changePosition = case finalMachine & _state & _position of
                               -- If the submachine tried to exit to an invalid
                               -- label, assume that label is a goto for the
                               -- external machine
                               Left x -> const x 
                               -- Otherwise, just increment the position
                               Right _ -> fmap succ

-- Finally, we define pure submachines, which would not be able to jump outside
-- of their own instruction set, and better encapsulate the concept of running
-- a coded RM inside a single register of a universal RM. The implementation
-- for "interpret" is identical to that of ordinary "macro" submachines, but
-- any goto behaviour is ignored.
newtype PureSubmachine instr values = Pure (Submachine instr values)

instance (Instruction instr values, Default values)
       => Instruction (PureSubmachine instr) values where
    interpret (Pure submachine) machinestate
      = machinestate
            & interpret submachine
              -- Overwrite goto behaviour from running the submachine, succ instead
            & L.set position (fmap succ $ _position machinestate)

-- Parse in a macro instruction, given a parser for the underlying label and instruction
parseMacro :: ReadP label -> ReadP (MacroData label)
parseMacro label = do
    string "macro"
    sp
    name <- readS_to_P reads
    registers <- many $ sp >> register
    labels <- many $ sp >> labelArgument
    pure $ MacroData name registers labels
    where
    labelArgument = do
        char '#'
        label

-- Read a macro
instance Read1 MacroData where
    liftReadsPrec readsPrec readList _
      = readP_to_S $ parseMacro (readS_to_P $ readsPrec 10)
