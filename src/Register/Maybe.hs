{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Register.Maybe where

import Prelude hiding (Maybe)
import qualified Control.Lens as L
import Control.Lens ((^.), (.~), (%~))
import Register
import Text.ParserCombinators.ReadP
import Data.Functor.Classes

data Maybe label = Maybe label
    deriving Functor
data Pair a = Pair { nojump :: a, jump :: a }
    deriving Functor

instance InstructionF Pair Maybe a where
    interpretF (Maybe label) machinestate
      = Pair
        (L.over position (fmap succ) machinestate)
        (L.set position label machinestate)

-- Parse in a regular instruction
parseInstr :: ReadP label -> ReadP (Maybe label)
parseInstr label = do
    string "maybe"
    sp
    label <- label
    pure $ Maybe label

instance Read1 Maybe where
    liftReadsPrec readsPrec _ _ = readP_to_S $ parseInstr (readS_to_P (readsPrec 10))

