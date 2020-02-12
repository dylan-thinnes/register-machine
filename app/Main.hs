{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Register
import Register.Instr
import Register.Maybe

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Data.Functor.Compose
import Control.Comonad.Trans.Cofree
import Data.Foldable
import Safe (atMay, readMay)
import Data.Default

-- Read a machine from stdin, run it, then report its registers
main :: IO ()
main = do
    args <- getArgs
    program <- read <$> getContents
    if "-t" `elem` args
       then runTrace program
       else runCW program

-- Report the registers to IO given a final machine
reportRegistersIO :: (Show value, Default value) => GMachine label instr value -> IO ()
reportRegistersIO = putStrLn . reportRegisters . _registers . _state

-- Run machine and print according to coursework spec
runCW :: Machine Label -> IO ()
runCW machine = reportRegistersIO $ runEnd machine

-- Run with full debug
runTrace :: Machine Label -> IO ()
runTrace machine = do
    let trace = run machine
    mapM_ (hPutStrLn stderr . debug True) trace
    reportRegistersIO $ last trace

-- Run interactively, giving the user the choice of movement down the tree
runInteractive :: forall f label instr value. (InstructionF f instr value, Foldable f, Applicative f, Functor instr, Default value, Eq label, Show value, Show label, Show (instr String))
               => GMachine label instr value
               -> IO ()
runInteractive machine = do
    finalState <- interact $ runF machine
    reportRegistersIO finalState
    where
        interact :: MachineTrace f (GMachine label instr value) -> IO (GMachine label instr value)
        interact (runCofree -> m :< rest) = do
            hPutStrLn stderr $ debug True m
            case getCompose rest of
              Just rest -> chooseBranch rest
              Nothing   -> hPutStrLn stderr "Machine halted." >> pure m
        chooseBranch f = prompt (toList f) >>= interact
        prompt choices = do
            hPutStrLn stderr $ "There are " ++ show (length choices) ++ " option(s)."
            result <- tryParseInput choices <$> getLine
            case result of
              Just res -> pure res
              Nothing  -> hPutStrLn stderr "Invalid input. Try again." >> prompt choices
        tryParseInput choices s = do
            ii <- readMay s
            choices `atMay` ii
