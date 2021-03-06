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
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Functor.Identity
import Data.Functor.Compose

import Data.Functor.Classes
import Data.Bitraversable
import Data.Bifunctor

import Control.Comonad.Trans.Cofree

import Text.Show.Deriving (deriveShow1)

-- ============================================================================
-- ================================ DATA TYPES ================================
-- ============================================================================

-- Registers
newtype Register = Register Integer
    deriving (Read, Enum, Eq, Ord, Num)

instance Show Register where
    show (Register r) = 'r' : show r

-- Simple string labels, using a newtype wrapper so we can write custom read
-- parsers for them
newtype Label = Label String
    deriving (Eq, Ord)

instance Show Label where
    show (Label s) = s

-- Add InstrSum constructors, which creates a new instruction set from two
-- previous instruction sets, f and g
newtype InstrSum f g a = InstrSum (Sum f g a)
    deriving (Show, Functor, Foldable, Traversable)
pattern InstrL x <- InstrSum (InL x) where
    InstrL x = InstrSum (InL x)
pattern InstrR x <- InstrSum (InR x) where
    InstrR x = InstrSum (InR x)

-- Simple isomorphism between InstrSum and Either
sumToEither :: InstrSum f g a -> Either (f a) (g a)
sumToEither (InstrL x) = Left x
sumToEither (InstrR x) = Right x

eitherToSum :: Either (f a) (g a) -> InstrSum f g a
eitherToSum (Left x) = InstrL x
eitherToSum (Right x) = InstrR x

-- Type of natural transformations
type f :-> g = forall a. f a -> g a

-- Define full transformation and natural transformations using the isomorphism
trans :: (f1 a -> f2 b) -> (g1 a -> g2 b) -> InstrSum f1 g1 a -> InstrSum f2 g2 b
trans f g = eitherToSum . bimap f g . sumToEither

natTrans :: (f1 :-> f2) -> (g1 :-> g2) -> InstrSum f1 g1 :-> InstrSum f2 g2
natTrans = trans

appNatTrans :: (Applicative m)
            => (forall label. f1 label -> m (f2 label))
            -> (forall label. g1 label -> m (g2 label))
            -> (forall label. InstrSum f1 g1 label -> m (InstrSum f2 g2 label))
appNatTrans f g = fmap eitherToSum . extractEitherF . bimap f g . sumToEither
    where
    extractEitherF :: (Applicative f) => Either (f a) (f b) -> f (Either a b)
    extractEitherF = bitraverse id id

injLNatTrans :: (Applicative m)
             => (forall label. i1 label -> m (i2 label))
             -> (forall label. InstrSum i1 right label -> m (InstrSum i2 right label))
injLNatTrans f = appNatTrans f pure

injRNatTrans :: (Applicative m)
             => (forall label. i1 label -> m (i2 label))
             -> (forall label. InstrSum left i1 label -> m (InstrSum left i2 label))
injRNatTrans g = appNatTrans pure g

-- Define Read1 instance for InstrSum, assuming both contained instructions are different
instance (Read1 i1, Read1 i2) => Read1 (InstrSum i1 i2) where
    liftReadsPrec readsPrec readList prec
      = readP_to_S
      $ choice [ fmap InstrL $ readS_to_P $ liftReadsPrec readsPrec readList prec
               , fmap InstrR $ readS_to_P $ liftReadsPrec readsPrec readList prec
               ]

-- Positions are just Integers, but we don't want them to be interchangeable so we use newtype
newtype Position = Position { unpos :: Integer }
    deriving (Read, Enum, Eq, Ord, Num)

instance Show Position where
    show = show . unpos

-- GCode - A synonym for a map containing Positions and their Instructions
-- The "label" type parameter is for jump instructions that have an unfound
-- target in the original assembly.
type GCode instr label = M.Map Position (instr (Either label Position))

-- GAssembly is a list of instructions, each with an optional label
-- src/Register/Samples.hs contains an example of handwritten assembly

-- This assembly is later turned into a Code that the Machine can run by
-- resolving the labels to positions.
newtype GAssembly instr label = Assembly { unassembly :: [(Maybe label, instr label)] }
    deriving (Show, Eq, Ord, Functor, Semigroup, Monoid, Foldable, Traversable)

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

-- Convert one type of intruction to another in an assembly using a natural
-- transformation
-- This transformation is composed with an arbitrary monad (e.g. IO)
convertInstrs :: (Functor i1, Functor i2, Monad m)
                 -- The natural transformation between MacroData and
                 -- Submachine, with no awareness of labels
              => (forall label. i1 label -> m (i2 label))
                 -- The final transformation from Macro to InstrSum and Submachine
              -> GAssembly i1 label
              -> m (GAssembly i2 label)
convertInstrs converter
  = let (|>) = flip (.) -- Simple "composition in reverse", just this once
     in  -- Unwrap
         unassembly
         -- Run the following functions on every Macro:
      |> map (fmap converter)
         -- Sequence out the monad nested in [(e, a)] in one step w/ Compose
      |> Compose |> sequence |> fmap getCompose
         -- Rewrap within the monad
      |> fmap Assembly

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

-- Transform the instructions to other instructions in a machine using a
-- natural transformation
transMachineInstrs :: (i1 :-> i2) -> GMachine label i1 values -> GMachine label i2 values
transMachineInstrs transformation = instructions %~ fmap transformation

-- We also define the Instruction typeclass, which represents instructions
-- which can be interpreted to transform a GMachine
class Instruction instr operands where
    interpret :: instr (Either label Position)
              -> GMachineState label operands
              -> GMachineState label operands

-- If we have two Instructions, their InstrSum is also a valid instruction
instance (Instruction i1 a, Instruction i2 a) => Instruction (InstrSum i1 i2) a where
    interpret (InstrL instr) machinestate = interpret instr machinestate
    interpret (InstrR instr) machinestate = interpret instr machinestate

-- For future expansion with NRM, we have the InstructionF typeclass, which
-- represents instructions which can put the register machine into a functor,
-- which represents multiple states.
class InstructionF f instr operands where
    interpretF :: instr (Either label Position)
              -> GMachineState label operands
              -> f (GMachineState label operands)

instance (InstructionF f i1 a, InstructionF f i2 a) => InstructionF f (InstrSum i1 i2) a where
    interpretF (InstrL instr) machinestate = interpretF instr machinestate
    interpretF (InstrR instr) machinestate = interpretF instr machinestate

-- Any ordinary Instruction instance can be turned into a InstructionF instance
-- for any Applicative functor if wrapped in the LiftedInstr newtype
newtype LiftedInstr instr label = LiftInstr { lowerInstr :: instr label }
    deriving (Show, Eq)
deriveShow1 ''LiftedInstr

instance Functor instr => Functor (LiftedInstr instr) where
    fmap f (LiftInstr i) = LiftInstr (fmap f i)

instance (Read1 instr) => Read1 (LiftedInstr instr) where
    liftReadsPrec readsPrec readList prec
      = readP_to_S . fmap LiftInstr . readS_to_P
      $ liftReadsPrec readsPrec readList prec

instance (Applicative f, Instruction instr operands)
    => InstructionF f (LiftedInstr instr) operands where
    interpretF (LiftInstr instr) = pure . interpret instr

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

-- Parse in a register
register = char 'r' >> fmap Register (readS_to_P reads)

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
reportRegisters positions = unwords $ "registers" : regs
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

-- Make a single step of the machine, optionally within a functor
nextF :: (Functor f, InstructionF f instr value)
      => GMachine label instr value
      -> Compose Maybe f (GMachine label instr value)
nextF machine = Compose $ do
    currInstr <- instruction machine
    let stateF = interpretF currInstr $ _state $ machine
    pure $ stateF <&> \state -> machine { _state = state }

next :: (Instruction instr value)
     => GMachine label instr value -> Maybe (GMachine label instr value)
next = fmap (transMachineInstrs lowerInstr)
     . getComposeRid . nextF
     . transMachineInstrs LiftInstr

-- Turn a KCoalgebra into a KCoalgebra for Cofree Comonad's Base functor
liftCofreeF :: (Functor f) => (a -> f b) -> a -> CofreeF f a b
liftCofreeF f a = a :< f a

-- Turn a combining function and KAlgebra into a KAlgebra for Cofree Comonad's Base functor
lowerCofreeF :: (Functor f) => (a -> c -> d) -> (f b -> c) -> CofreeF f a b -> d
lowerCofreeF f g (a :< b) = f a (g b)

getComposeRid :: (Functor f) => Compose f Identity a -> f a
getComposeRid = fmap runIdentity . getCompose
getComposeLid :: Compose Identity g a -> g a
getComposeLid = runIdentity . getCompose

-- Unfold GMachine into Cofree f GMachine using recursion-schemes, assuming
-- that its instruction set belongs to the InstructionF typeclass.
runF :: (InstructionF f instr value, Functor f)
     => GMachine label instr value
     -> Cofree (Compose Maybe f) (GMachine label instr value)
runF = F.ana $ Compose . Identity . liftCofreeF nextF

-- Convenient type synonym for roses that runF would produce with a list functor
type MachineTrace f = Cofree (Compose Maybe f)
type MachineRose = Cofree (Compose Maybe [])

-- Hylomorph the Cofree Maybe functor to a List
run :: (Instruction instr value)
    => GMachine label instr value -> [GMachine label instr value]
run = F.hylo (lowerCofreeF (:) (fromMaybe [])) (liftCofreeF next)

runEnd :: (Instruction instr value)
       => GMachine label instr value -> GMachine label instr value
runEnd = last . run
