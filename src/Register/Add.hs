{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Register.Add where

import Register
import Control.Lens ((^.), (.~), (%~))
import Data.Default
import Data.Function ((&))
import Text.ParserCombinators.ReadP
import Data.Functor.Classes

data Add label = Add { in1 :: Register, in2 :: Register, out :: Register }
    deriving (Show, Read, Eq, Ord, Functor)

instance (Num a, Default a) => Instruction Add a where
    interpret (Add {..}) machine
      = machine
      & statelens out .~ (machine ^. statelens in1 + machine ^. statelens in2)

-- Parse in a regular instruction
parseAdd :: ReadP (Add label)
parseAdd = do
    string "add"
    sp
    in1 <- register
    sp
    in2 <- register
    sp
    out <- register
    pure $ Add {..}

instance Read1 Add where
    liftReadsPrec readsPrec _ _ = readP_to_S parseAdd

