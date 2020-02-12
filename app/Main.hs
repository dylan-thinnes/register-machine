module Main where

import Register
import Register.Instr

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

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

