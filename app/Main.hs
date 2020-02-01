module Main where

import Register
import Register.Instr

-- Read a machine from stdin, run it, then report its registers
main :: IO ()
main = fmap read getContents >>= runCW

-- Run machine and print according to coursework spec
runCW :: Machine Label -> IO ()
runCW machine = putStrLn $ reportRegisters $ _registers $ _state $ run machine
