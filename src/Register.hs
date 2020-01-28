{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Register where

import qualified Data.Map as M
import Data.Map ((!?))
import qualified Data.Set as S
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Foldable (fold)

import qualified Control.Lens as L
import Control.Lens ((^.), (.~), (%~))

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isAlpha, isAlphaNum)
import Data.List (intersperse)
import Control.Monad (liftM, liftM2)

import Data.Default

import qualified Data.Functor.Foldable as F
import Data.Functor.Sum

import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Bitraversable
import Data.Bifunctor

-- ============================================================================
-- ================================ DATA TYPES ================================
-- ============================================================================

-- Registers
newtype Register = Register Integer
    deriving (Read, Enum, Eq, Ord, Num)

instance Show Register where
    show (Register r) = 'r' : show r

-- Instructions, with choice of any value for labels
data Instr label = Decjz Register label | Inc Register
    deriving (Show, Read, Eq, Ord, Functor)

-- Simple string labels, using a newtype wrapper so we can write custom read
-- parsers for them
newtype Label = Label String
    deriving (Show, Eq, Ord)

-- Add InstrSum constructors, which creates a new instruction set from two
-- previous instruction sets
newtype InstrSum f g a = InstrSum (Sum f g a)
    deriving (Functor)
pattern InstrL x <- InstrSum (InL x) where
    InstrL x = InstrSum (InL x)
pattern InstrR x <- InstrSum (InR x) where
    InstrR x = InstrSum (InR x)

type f :-> g = forall a. f a -> g a

trans :: (f1 a -> f2 b) -> (g1 a -> g2 b) -> InstrSum f1 g1 a -> InstrSum f2 g2 b
trans f _ (InstrL x) = InstrL $ f x
trans _ g (InstrR x) = InstrR $ g x

natTrans :: (f1 :-> f2) -> (g1 :-> g2) -> InstrSum f1 g1 :-> InstrSum f2 g2
natTrans = trans

sumToEither :: InstrSum f g a -> Either (f a) (g a)
sumToEither (InstrL x) = Left x
sumToEither (InstrR x) = Right x

eitherToSum :: Either (f a) (g a) -> InstrSum f g a
eitherToSum (Left x) = InstrL x
eitherToSum (Right x) = InstrR x

extractEitherF :: (Applicative f) => Either (f a) (f b) -> f (Either a b)
extractEitherF = bitraverse id id

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
    deriving (Eq, Ord, Functor)

-- Submachines encapsulate the concept of running a register machine inside of
-- a single register machine
-- They are useful for defining macros
data Submachine instr label
    = Submachine 
    { -- The GCode defining the machine
      gcode :: GCode instr label
      -- Registers to take as inputs from the current machine state, mapped to R0, R1, etc.
    , inputRegs :: M.Map Register Register
    , inputLabels :: M.Map label label
      -- Registers to send as outputs (key is register in macro to be read
      -- from, value is register in macro to be sent to)
    , resultRegs :: M.Map Register Register
    }

-- Positions are just Integers, but we don't want them to be interchangeable so we use newtype
newtype Position = Position { unpos :: Integer }
    deriving (Show, Read, Enum, Eq, Ord, Num)

-- GCode - A synonym for a map containing Positions and their Instructions
-- The "label" type parameter is for jump instructions that have an unfound
-- target in the original assembly.
type GCode instr label = M.Map Position (instr (Either label Position))
type Code label = GCode Instr label

-- GAssembly is a list of instructions, each with an optional label
-- src/Register/Samples.hs contains an example of handwritten assembly

-- This assembly is later turned into a Code that the Machine can run by
-- resolving the labels to positions.
newtype GAssembly instr label = Assembly { unassembly :: [(Maybe label, instr label)] }
    deriving (Show, Eq, Ord, Functor, Semigroup, Monoid, Foldable, Traversable)

type Assembly = GAssembly Instr
type MGAssembly instr = GAssembly (Macro instr)
type MAssembly = MGAssembly Instr

-- Turn assembly into code
assemble :: (Ord label, Functor instr) => GAssembly instr label -> GCode instr label
assemble (Assembly assembly)
  = M.fromList $ zip [0..] $ map (f . snd) assembly
    where
    f = fmap $ \label -> toEither label $ labelIndices !? label
    -- If there is no value, default to Left def
    toEither def Nothing = Left def
    toEither _ (Just y) = Right y
    -- Calculate the index for each label
    labelIndices
      = M.fromList
      $ catMaybes
      $ zipWith (\i -> fmap (,i)) [0..]
      $ map fst assembly

-- Join two assemblies, end to end
-- Just a synonym for the monoid on lists
join :: GAssembly instr label -> GAssembly instr label -> GAssembly instr label
join = (<>)

-- Join multiple assemblies, end to end to end to...
-- Just a synonym for fold using the monoid on lists
joinAll :: [GAssembly instr label] -> GAssembly instr label
joinAll = fold

-- Merge different assemblies without chance of misreferencing
mergeMany :: (Functor instr)
          => [GAssembly instr label] -> GAssembly instr (Int, label)
mergeMany assemblies 
  = assemblies
    -- Assign an index to each assembly's labels
  & zipWith (\i assembly -> fmap (i,) assembly) [0..]
    -- Join all assemblies
  & joinAll

-- Merge two assemblies, even of different types
mergeTwo :: (Functor instr)
         => GAssembly instr label1 -> GAssembly instr label2
         -> GAssembly instr (Either label1 label2)
mergeTwo a b = join (fmap Left a) (fmap Right b)

-- Resolve the macros in an assembly using a natural transformation
-- This transformation is composed with an arbitrary monad (e.g. IO)
resolveMacros :: (Functor i1, Functor i2, Monad m)
                 -- The natural transformation between MacroData and
                 -- Submachine, with no awareness of labels
              => (MacroData :-> Compose m (Submachine i1))
                 -- The final transformation from Macro to InstrSum and Submachine
              -> GAssembly (Macro i2) label
              -> m (GAssembly (InstrSum (Submachine i1) i2) label)
resolveMacros converter
  = let (|>) = flip (.) -- Simple "composition in reverse", just this once
     in  -- Unwrap
         unassembly
         -- Run the following functions on every Macro:
      |> map ( fmap
          (  natTrans converter id   -- Run the natural transformation
          |> sumToEither             -- Temporarily turn the sum into an either
          |> bimap getCompose pure   -- Turn both values into m monad
          |> bitraverse id id        -- Bitraverse the monad out from the either
          |> fmap eitherToSum        -- Turn the either back into a sum
          ))
         -- Sequence out the monad in one step w/ Compose
      |> Compose |> sequence |> fmap getCompose
         -- Rewrap within the monad
      |> fmap Assembly

-- Resolve the macros with a pure natural transformation
resolveMacrosPure :: (Functor i1, Functor i2)
                     -- The natural transformation between MacroData and
                     -- Submachine, with no awareness of labels
                  => (MacroData :-> Submachine i1)
                     -- The final transformation from Macro to InstrSum and Submachine
                  -> GAssembly (Macro i2) label
                  -> GAssembly (InstrSum (Submachine i1) i2) label
resolveMacrosPure converter = runIdentity . resolveMacros (Compose . Identity . converter)

-- Machine - puts all of the types together
-- We keep the instruction and value types general so that we can manipulate
-- any codeable value later, and upgrade our instruction set to have oracles
-- and macros
data GMachine label instr values
    = GM
    { _instructions :: GCode instr label
    , _state :: GMachineState label values
    }

-- Separate out machine state so that separate instructions can manipulate the
-- same state (refer to Instruction typeclass for Sum)
data GMachineState label values
    = GMState
    { _registers :: M.Map Register values
    , _position :: Either label Position
    }

type Machine label = GMachine label Instr Integer

-- Create lenses for easily getting/setting/modifying parts of a machine
L.makeLenses ''GMachine
L.makeLenses ''GMachineState

-- Create pattern for getting all fields from machine
pattern GMachineAll _instructions _registers _position <- GM _instructions (GMState _registers _position) where
    GMachineAll _instructions _registers _position = GM _instructions (GMState _registers _position)

-- Get the current instruction of the machine, if there is any at the machine position
instruction :: GMachine label instr values -> Maybe (instr (Either label Position))
instruction (GMachineAll instructions _ position)
  = case position of
      Left e -> Nothing
      Right p -> instructions !? p

-- Lens for taking a machine to a single register value
lens :: (Default value) => Register -> L.Lens' (GMachine label instr value) value
lens r = state . registers . sublens r

-- Lens for taking a machine state to a single register value
statelens :: (Default value) => Register -> L.Lens' (GMachineState label value) value
statelens r = registers . sublens r

-- Lens for taking a map of register values to a single register value
sublens :: (Default value) => Register -> L.Lens' (M.Map Register value) value
sublens r = L.lens (fromMaybe def . M.lookup r) (\m i -> M.insert r i m)
-- Could use this sublens, but use of L.non means it removes keys w/ value 0
-- sublens r = L.at r . L.non 0

-- We also define the Instruction typeclass, which represents instructions
-- which can be interpreted to transform a GMachine
class Instruction instr operands where
    interpret :: instr (Either label Position)
              -> GMachineState label operands
              -> GMachineState label operands

-- If we have two Instructions, their sum is also a valid instruction
instance (Instruction i1 a, Instruction i2 a) => Instruction (InstrSum i1 i2) a where
    interpret (InstrL instr) machinestate = interpret instr machinestate
    interpret (InstrR instr) machinestate = interpret instr machinestate

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
            finalMachine = run $ GM gcode initialState
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

-- ============================================================================
-- ============================ READING AND PARSING ===========================
-- ============================================================================
-- We use the ReadP parser combinator to parse in the assembly / machine easily

-- Helper for parsing - either runs the parser, returning Just a, or returns
-- Nothing at all
maybeParse :: ReadP a -> ReadP (Maybe a)
maybeParse = option Nothing . fmap Just

-- Parsers for SP and NL parts of grammar
sp = munch1 (`elem` " \t")
nl = do
    char '\n'
    optional $ do
        char '#'
        munch ('\n' /=)
        char '\n'

-- Parsers for identifiers and labels
identifier :: ReadP String
identifier = liftM2 (:) (satisfy isAlpha) (munch isAlphaNum)

label :: ReadP Label
label = fmap Label identifier

-- Parse assembly with a string
parseStringAssembly :: ReadP (Assembly Label)
parseStringAssembly = parseGAssembly (parseInstr label) label

-- Parse in a macro instruction, given a parser for the underlying label and instruction
parseMacro :: ReadP label -> ReadP (instr label) -> ReadP (Macro instr label)
parseMacro label inst = choice [macro, fmap NMacro inst]
    where
    macro = do
        string "macro"
        sp
        name <- identifier
        registers <- many $ sp >> register
        labels <- many $ sp >> labelArgument
        pure $ YMacro $ MacroData name registers labels

    labelArgument = do
        char '#'
        label

instance (Read1 instr) => Read1 (Macro instr) where
    liftReadsPrec readsPrec readList _
      = readP_to_S $ parseMacro (readS_to_P $ readsPrec 10)
      $ readS_to_P $ liftReadsPrec readsPrec readList 10

-- Parse in a register
register = char 'r' >> fmap Register (readS_to_P reads)

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

-- Parser for assembly with parseable labels and parseable instructions
parseGAssembly :: forall instr label.
                  ReadP (instr label)
               -> ReadP label
               -> ReadP (GAssembly instr label)
parseGAssembly inst label = fmap Assembly labInsts
    where
    -- Parse in multiple labInst, with NL separating
    labInsts = many $ do
        labInst <- labInst
        nl
        pure labInst

    -- Parse in a labInst (label : instruction) pair
    labInst :: ReadP (Maybe label, instr label)
    labInst = do
        label <- maybeParse $ do
            label <- label
            optional sp
            char ':'
            pure label
        optional sp
        inst <- inst
        pure (label, inst)

instance Read Label where
    readsPrec _ = readP_to_S label

-- Define read instance for Assembly using parser combinator above
instance (Read1 instr, Read label) => Read (GAssembly instr label) where
    readsPrec _ = readP_to_S $ parseGAssembly (readS_to_P $ liftReadsPrec readsPrec readList 10) (readS_to_P reads)

-- Define read instance for GMachine
instance (Functor instr, Read1 instr, Read values) => Read (GMachine Label instr values) where
    readsPrec _ = readP_to_S $ do
        -- Parse 0 or more registers
        string "registers"
        rs <- many (sp >> readS_to_P reads)
        let registers = M.fromList $ zip [0..] rs
        -- skip newline
        nl
        -- Parse assembly
        assembly <- readS_to_P reads :: ReadP (GAssembly instr Label)
        -- Return machine initialized at Position 0
        pure $ GMachineAll (assemble assembly) registers (Right $ Position 0)

-- ============================================================================
-- ============================ PRINTING AND DEBUGGING ========================
-- ============================================================================

instance (Show (instr label), Show label) => Show (Macro instr label) where
    show (NMacro x) = show x
    show (YMacro (MacroData x registers labels))
      = unwords $ ["Macro", show x] ++ map show registers ++ map showLabel labels
        where
            showLabel x = '#' : show x

-- Debug will print out a machine as registers and instructions
-- Optionally, if markPosition is set, it will put a '*' next to the line
-- currently being evaluated.
debug :: forall label instr value.
         (Functor instr, Show (instr String), Show value, Eq label, Show label)
      => Bool -> GMachine label instr value -> String
debug markPosition (GMachineAll _instructions _registers _position)
  = unlines $ registers : instructions
      where
        -- Get maximum position, to pad all position numbers correctly
        maxPos = fromMaybe 0 $ unpos . fst <$> M.lookupMax _instructions
        l = length $ show maxPos
        pad s = replicate (l - length s) ' ' ++ s

        -- Show registers as <reg>, <value> pairs
        registers = concat $ intersperse ", " $ (fmap showReg $ M.toList _registers)
        showReg (r,v) = show r ++ ": " ++ show v

        -- Show instructions with line numbers and a '*' marking the current line
        instructions = fmap showLine $ M.toList _instructions
        showLine :: (Position, instr (Either label Position)) -> String
        showLine (pos, instr)
          = (if markPosition && Right pos == _position then "*" else " ")
          ++ pad (show pos)
          ++ " : " ++ show (fmap showLabel instr)
        showLabel (Left label) = '!' : show label
        showLabel (Right pos) = show $ unpos pos

-- Report the registers as according to the coursework spec
reportRegisters :: (Show value, Default value) => M.Map Register value -> String
reportRegisters positions = "registers " ++ unwords regs
    where
    -- Lookup the register for each position, defaulting to 0 if it isn't there
    regs = map (\r -> show $ L.view (sublens r) positions) regNums
    -- Create a list of positive register positions to show. If the map is
    -- empty, have an empty list.
    regNums = [0 .. fromMaybe (-1) $ fst <$> M.lookupMax positions]

-- Define the show instance of the machine as show without the '*' marking the
-- position
instance (Eq label, Show label, Functor instr, Show (instr String), Show value) 
      => Show (GMachine label instr value) where
    show = debug False

-- ============================================================================
-- ============================ RUNNING AND STEPPING ==========================
-- ============================================================================

-- Make a single step of the machine
next :: (Instruction instr value)
     => GMachine label instr value -> Maybe (GMachine label instr value)
next machine = do
    currInstr <- instruction machine
    pure $ machine & state %~ interpret currInstr

-- Turn maybes into the base functor for a list
-- This lets us use recursion-schemes for unfolding the states of the machine
toListF :: Maybe a -> F.ListF a a
toListF (Just x) = F.Cons x x
toListF Nothing  = F.Nil

-- Unfold ListF Machine into [Machine], using recursion-schemes, then take the
-- last machine state
-- Using recursion-schemes lets us eventually upgrade our machine transitions
-- to be probabilistic
run :: (Instruction instr value)
    => GMachine label instr value -> GMachine label instr value
run initial = last $ initial : F.unfold (toListF . next) initial
