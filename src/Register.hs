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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Numbers.Primes (primes, primeFactors)
import Data.List (groupBy, group)

import Data.Functor.Classes

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

data Macro instr label = Macro String [Register] [label] | NoMacro (instr label)
    deriving (Eq, Ord)

instance (Functor instr) => Functor (Macro instr) where
    fmap f (Macro name regs labels) = Macro name regs (fmap f labels)
    fmap f (NoMacro instr) = NoMacro (fmap f instr)

-- Positions are just Integers, but we don't want them to be interchangeable so we use newtype
newtype Position = Position { unpos :: Integer }
    deriving (Show, Read, Enum, Eq, Ord, Num)

-- GCode - Just a synonym for a map containing Positions and their Instructions
type GCode instr = M.Map Position (instr Position)
type Code = GCode Instr

-- GAssembly is a list of instructions, each with an optional label
-- src/Register/Samples.hs contains an example of handwritten assembly

-- This assembly is later turned into a Code that the Machine can run by
-- resolving the labels to positions.
newtype GAssembly instr label = Assembly { unassembly :: [(Maybe label, instr label)] }
    deriving (Show, Eq, Ord, Functor, Semigroup, Monoid)

newtype Label = Label String
    deriving (Show, Eq, Ord)

type Assembly = GAssembly Instr
type MGAssembly instr = GAssembly (Macro instr)
type MAssembly = MGAssembly Instr

-- Turn assembly into code
assemble :: (Ord a, Functor instr) => GAssembly instr a -> GCode instr
assemble (Assembly assembly)
  = M.fromList $ zip [0..] $ map (f . snd) assembly
    where
    f = fmap $ fromMaybe (-1) . (labelIndices !?)
    -- Calculate the index for each label
    labelIndices
      = M.fromList
      $ catMaybes
      $ zipWith (\i -> fmap (,i)) [0..]
      $ map fst assembly

-- Join two assemblies, end to end
-- Just a synonym for the monoid on lists
join :: GAssembly instr a -> GAssembly instr a -> GAssembly instr a
join = (<>)

-- Join multiple assemblies, end to end to end to...
-- Just a synonym for fold using the monoid on lists
joinAll :: [GAssembly instr a] -> GAssembly instr a
joinAll = fold

-- Merge different assemblies without chance of misreferencing
mergeMany :: (Functor instr)
          => [GAssembly instr a] -> GAssembly instr (Int, a)
mergeMany assemblies 
  = assemblies
    -- Assign an index to each assembly's labels
  & zipWith (\i assembly -> fmap (i,) assembly) [0..]
    -- Join all assemblies
  & joinAll

-- Merge two assemblies, even of different types
mergeTwo :: (Functor instr)
         => GAssembly instr a -> GAssembly instr b -> GAssembly instr (Either a b)
mergeTwo a b = join (fmap Left a) (fmap Right b)

-- Machine - puts all of the types together
-- We keep the instruction and value types general so that we can manipulate
-- any codeable value later, and upgrade our instruction set to have oracles
data GMachine instr values
    = RM
    { _instructions :: GCode instr
    , _state :: GMachineState values
    }

data GMachineState values
    = RMState
    { _registers :: M.Map Register values
    , _position :: Position
    }

type Machine = GMachine Instr Integer

-- Create lenses for easily getting/setting/modifying parts of a machine
L.makeLenses ''GMachine
L.makeLenses ''GMachineState

-- Create pattern for getting all fields from machine
pattern GMachineAll _instructions _registers _position <- RM _instructions (RMState _registers _position) where
    GMachineAll _instructions _registers _position = RM _instructions (RMState _registers _position)

-- Get the current instruction of the machine, if there is any at the machine position
instruction :: GMachine instr values -> Maybe (instr Position)
instruction (GMachineAll instructions _ position) = instructions !? position

-- Lens for taking a machine to a single register value
lens :: (Default value) => Register -> L.Lens' (GMachine instr value) value
lens r = state . registers . sublens r

-- Lens for taking a machine state to a single register value
statelens :: (Default value) => Register -> L.Lens' (GMachineState value) value
statelens r = registers . sublens r

-- Lens for taking a map of register values to a single register value
sublens :: (Default value) => Register -> L.Lens' (M.Map Register value) value
sublens r = L.lens (fromMaybe def . M.lookup r) (\m i -> M.insert r i m)
-- Could use this sublens, but use of L.non means it removes keys w/ value 0
-- sublens r = L.at r . L.non 0

-- We also define the Instruction typeclass, which represents instructions
-- which can be interpreted to transform a GMachine
class Instruction instr operands where
    interpret :: instr Position
              -> GMachineState operands
              -> GMachineState operands

-- If we have two Instructions, their sum is also a valid instruction
instance (Instruction i1 a, Instruction i2 a) => Instruction (Sum i1 i2) a where
    interpret (InL instr) machine = interpret instr machine
    interpret (InR instr) machine = interpret instr machine

-- Define Codeable values
class Codeable a where
    encode :: a -> Integer
    decode :: Integer -> a

instance Codeable Integer where
    encode = id
    decode = id

editCode :: Codeable a => (Integer -> Integer) -> a -> a
editCode f = decode . f . encode

-- Use factorPowers to implement 2^x * 3^y... etc coding functions
factorPowers :: Integer -> [(Integer, Integer)]
factorPowers x = x
               & primeFactors
               & group
               & map (\x -> (head x, fromIntegral $ length x))

-- Define generic codeable instances for most values we could want.
instance (Codeable a) => Codeable [a] where
    -- Use primes to code lists - The 2 ^ head * 3 ^ tail coding system would
    -- be much prettier, but would be prohibitive from a number size
    -- perspective even for the smallest inputs.
    encode xs = product $ zipWith (^) primes $ map encode xs
    decode x = map decode $ f (factorPowers x) primes
        where
            f [] _ = []
            f ((factor, power):factors) (prime : primes)
                | factor == prime = power : f factors primes
                | otherwise       = 0 : f ((factor, power):factors) primes

instance (Codeable a, Codeable b) => Codeable (a, b) where
    encode (a, b) = 2 ^ encode a * 3 ^ encode b
    decode x = let (a:b:_) = decode x ++ [0..]
                in (decode a, decode b)

instance (Codeable a, Codeable b) => Codeable (Either a b) where
    encode x = case x of
                 Left a -> 3 ^ encode a
                 Right a -> 2 * 3 ^ encode a
    decode x = let (a:b:_) = decode x ++ [0..]
                in if a == 0 then Left (decode b) else Right (decode b)

instance (Codeable a) => Codeable (Instr a) where
    encode (Inc (Register r)) = encode [1, encode r]
    encode (Decjz (Register r) label) = encode [2, encode r, encode label]
    decode x = let (a:b:c:_) = decode x ++ [0..]
                in if a == 1
                      then Inc $ Register $ decode b
                      else Decjz (Register $ decode b) (decode c)

-- We can define codeable instances for the Machine later...

-- Now we define that Instrs can manipulate any codeable value, which includes
-- plain old integers
instance (Codeable a, Default a) => Instruction Instr a where
    interpret instr machine = case instr of
      Decjz (statelens -> reg) label        -- If the instruction is a jump...
        -> if encode (L.view reg machine) == 0
            then machine                        -- If the register is 0 / undefined...
                    & L.set  reg (decode 0)         -- Set the register to 0
                    & L.set  position label         -- Jump to the label

            else machine                        -- If the register is not 0 / undefined...
                    & L.over reg (editCode pred)    -- Decrement the register
                    & L.over position succ          -- Go to the successive position

      Inc (statelens -> reg)                -- If the instruction is an increment...
        -> machine
            & L.over reg (editCode succ)        -- Increment the register
            & L.over position succ              -- Increment the position

-- Submachines encapsulate the concept of running a register machine inside of
-- a single register machine
-- They are useful for defining macros
data Submachine instr labels
    = Submachine 
    { -- The GCode defining the machien
      gcode :: GCode instr
      -- Registers to take as inputs from the current machine state, mapped to R0, R1, etc.
    , inputRegs :: [Register]
      -- Registers to send as outputs (key is register in macro to be read
      -- from, value is register in macro to be sent to)
    , resultRegs :: M.Map Register Register
    }

mapWithMap :: Ord k => M.Map k k -> M.Map k a -> M.Map k a
mapWithMap keysMapping = M.mapMaybeWithKey (\k a -> const a <$> M.lookup k keysMapping)

restrictWithList :: (Ord k, Enum k, Num k) => [k] -> M.Map k a -> M.Map k a
restrictWithList keys = mapWithMap keysMapping
    where
        keysMapping = M.fromList $ zip keys [0..]

instance (Instruction instr values, Default values) => Instruction (Submachine instr) values where
    interpret (Submachine {..}) machinestate
      = machinestate
              -- Overwrite registers in machine that were set in the macro
            & L.over registers (M.union finalRegisters)
              -- Increment the current position
            & L.over position succ
        where
            initialState = RMState (restrictWithList inputRegs $ _registers machinestate)
                                   (Position 0)
            finalMachine = run $ RM gcode initialState
            finalRegisters = finalMachine
                           & _state & _registers
                           & mapWithMap resultRegs

-- ============================================================================
-- ============================ READING AND PARSING ===========================
-- ============================================================================
-- We use the ReadP parser combinator to parse in the assembly / machine easily

-- Parser SP and NL parts of grammar
sp = munch1 (`elem` " \t")
nl = do
    char '\n'
    optional $ do
        char '#'
        munch ('\n' /=)
        char '\n'

-- Helper for parsing - either runs the parser, returning Just a, or returns
-- Nothing at all
maybeParse :: ReadP a -> ReadP (Maybe a)
maybeParse = option Nothing . fmap Just

-- Parse identifiers and labels
identifier :: ReadP String
identifier = liftM2 (:) (satisfy isAlpha) (munch isAlphaNum)

label :: ReadP Label
label = fmap Label identifier

-- Parse assembly with a string
parseStringAssembly :: ReadP (Assembly Label)
parseStringAssembly = parseGAssembly (parseInstr label) label

-- Parse in a macro, given a parser for the instruction
parseMacro :: ReadP (instr label) -> ReadP (Macro instr label)
parseMacro inst = choice [macro, fmap NoMacro inst]
    where
    macro = do
        string "macro"
        sp
        name <- identifier
        rs <- many $ sp >> register
        pure $ Macro name rs []

instance (Read1 instr) => Read1 (Macro instr) where
    liftReadsPrec readsPrec readList _
      = readP_to_S $ parseMacro 
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
instance (Functor instr, Read1 instr, Read values) => Read (GMachine instr values) where
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
        pure $ GMachineAll (assemble assembly) registers (Position 0)

-- ============================================================================
-- ============================ PRINTING AND DEBUGGING ========================
-- ============================================================================

instance (Show (instr label), Show label) => Show (Macro instr label) where
    show (NoMacro x) = show x
    show (Macro x xs ys) = unwords $ ["Macro", show x] ++ map show xs ++ map show ys

-- Debug will print out a machine as registers and instructions
-- Optionally, if markPosition is set, it will put a '*' next to the line
-- currently being evaluated.
debug :: (Functor instr, Show (instr Integer), Show value)
      => Bool -> GMachine instr value -> String
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
        showLine (Position pos, instr)
          = (if markPosition && pos == unpos _position then "*" else " ")
          ++ pad (show pos)
          ++ " : " ++ show (fmap unpos instr)

-- Report the registers as according to the coursework spec
reportRegisters :: M.Map Register Integer -> String
reportRegisters positions = "registers " ++ unwords regs
    where
    -- Lookup the register for each position, defaulting to 0 if it isn't there
    regs = map (\r -> show $ L.view (sublens r) positions) regNums
    -- Create a list of positive register positions to show. If the map is
    -- empty, have an empty list.
    regNums = [0 .. fromMaybe (-1) $ fst <$> M.lookupMax positions]

-- Define the show instance of the machine as show without the '*' marking the
-- position
-- This allows read and show to go between the same machine and source Code
instance (Functor instr, Show (instr Integer), Show value) 
      => Show (GMachine instr value) where
    show = debug False

-- ============================================================================
-- ============================ RUNNING AND STEPPING ==========================
-- ============================================================================

-- Make a single step of the machine
next :: (Instruction instr value)
     => GMachine instr value -> Maybe (GMachine instr value)
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
    => GMachine instr value -> GMachine instr value
run initial = last $ initial : F.unfold (toListF . next) initial
