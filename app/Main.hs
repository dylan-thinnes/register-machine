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

-- Run machine and print according to coursework spec
runCW :: Machine Label -> IO ()
runCW machine = putStrLn $ reportRegisters $ _registers $ _state $ runEnd machine

-- Run with full debug
runTrace :: Machine Label -> IO ()
runTrace machine = do
    let trace = run machine
    mapM_ (hPutStrLn stderr . debug True) trace
    putStrLn $ reportRegisters $ _registers $ _state $ last trace

