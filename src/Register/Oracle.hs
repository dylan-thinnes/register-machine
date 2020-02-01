{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Register.Oracle where

import Register
import Data.Default
import qualified Control.Lens as L

data Oracle value label = Oracle Register (value -> value)

instance (Default value) => Instruction (Oracle value) value where
    interpret (Oracle register f) = L.over (statelens register) f
