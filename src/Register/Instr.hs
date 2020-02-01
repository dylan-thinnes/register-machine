{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Register.Instr where

import Register
import Data.Functor.Classes
import Text.ParserCombinators.ReadP
import qualified Control.Lens as L
import Data.Function ((&))

-- Instructions, with choice of any value for labels
data Instr label = Decjz Register label | Inc Register
    deriving (Show, Read, Eq, Ord, Functor)

type Machine label = GMachine label Instr Integer
type Code label = GCode Instr label
type Assembly = GAssembly Instr

-- Parse assembly with a string
parseStringAssembly :: ReadP (Assembly Label)
parseStringAssembly = parseGAssembly (parseInstr label) label

-- Now we define that Instrs can manipulate any integer value, which includes
-- plain old integers
instance Instruction Instr Integer where
    interpret instr machine = case instr of
      Decjz (statelens -> reg) label        -- If the instruction is a jump...
        -> if L.view reg machine == 0
            then machine                        -- If the register is 0 / undefined...
                    & L.set  reg 0                  -- Set the register to 0
                    & L.set  position label         -- Jump to the label

            else machine                        -- If the register is not 0 / undefined...
                    & L.over reg pred               -- Decrement the register
                    & L.over position (fmap succ)   -- Go to the successive position

      Inc (statelens -> reg)                -- If the instruction is an increment...
        -> machine
            & L.over reg succ                   -- Increment the register
            & L.over position (fmap succ)       -- Increment the position

-- Parse in a regular instruction
parseInstr :: ReadP label -> ReadP (Instr label)
parseInstr label = choice [inc, decjz]
    where
    -- Parse in an inc
    inc = do
        string "inc"
        sp
        r <- register
        pure $ Inc r

    -- Parse in a decjz
    decjz = do
        string "decjz"
        sp
        r <- register
        sp
        label <- label
        pure $ Decjz r label

instance Read1 Instr where
    liftReadsPrec readsPrec _ _ = readP_to_S $ parseInstr (readS_to_P (readsPrec 10))

