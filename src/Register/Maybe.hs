{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Register.Maybe where

import Prelude hiding (Maybe)
import qualified Control.Lens as L
import Control.Lens ((^.), (.~), (%~))
import Register
import Text.ParserCombinators.ReadP
import Data.Functor.Classes
import Text.Show.Deriving (deriveShow1)

data Maybe label = Maybe label
    deriving (Show, Functor)
deriveShow1 ''Maybe

-- Isomorphic to an explicitly recursive binary tree...
data Branch a = Split { nojump :: Branch a, jump :: Branch a } | Straight a
    deriving (Foldable, Functor, Show)

instance Applicative Branch where
    pure = Straight
    (Straight f) <*> x = fmap f x
    (Split f g) <*> x = Split (f <*> x) (g <*> x)

-- Simple, broken instance of Show1 for Branch:
instance Show1 Branch where
    liftShowsPrec showsPrec showList prec (Split {..})
      = ("Split {nojump = " ++)
      . liftShowsPrec showsPrec showList 10 nojump
      . (", jump = " ++)
      . liftShowsPrec showsPrec showList 10 jump
      . ("}" ++)
    liftShowsPrec showsPrec showList prec (Straight a)
      = ("Straight (" ++) . showsPrec 10 a . (")" ++)

instance InstructionF Branch Maybe a where
    interpretF (Maybe label) machinestate
      = Split { nojump = Straight $ L.over position (fmap succ) machinestate
              , jump   = Straight $ L.set position label machinestate
              }

-- Parse in a maybe
parseMaybe :: ReadP label -> ReadP (Maybe label)
parseMaybe label = do
    string "maybe"
    sp
    label <- label
    pure $ Maybe label

instance Read1 Maybe where
    liftReadsPrec readsPrec _ _ = readP_to_S $ parseMaybe (readS_to_P (readsPrec 10))

