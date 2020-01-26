module Register.Samples where

import Register

import qualified Data.Map as M

-- Testing
test = GMachineAll instrs registers 0
    where
    registers = M.empty
    instrs = M.fromList $ zip [0..]
        [ Inc 0
        , Inc 0
        , Inc 1
        ]

example :: Machine
example = read $ unlines
    [ "registers 10 5"
    , "loop : decjz r1 HALT"
    , "       decjz r0 HALT"
    , "       decjz r2 loop"
    ]

-- Square Program
square i = GMachineAll instrs registers 0
    where
    registers = M.fromList $
        [(Register 0, i)
        ]
    instrs = assemble $ Assembly
         [(Just "l1", Decjz 0 "l2"
        ),(Nothing,     Inc (-1)
        ),(Nothing,     Inc (-2)
        ),(Nothing,     Decjz (-4) "l1" -- uncond
        ),(Just "l2", Decjz (-1) "l3"
        ),(Just "l4",   Decjz (-2) "l5"
        ),(Nothing,       Inc 0
        ),(Nothing,       Inc (-3)
        ),(Nothing,       Decjz (-4) "l4" -- uncond
        ),(Just "l5",   Decjz (-3) "l2"
        ),(Nothing,       Inc (-2)
        ),(Nothing,       Decjz (-4) "l5" -- uncond
        ),(Just "l3", Decjz (-2) "end"
        ),(Nothing,     Decjz (-4) "l3" -- uncond
        )]

squareReadAssembly i = read $ squareReadAssemblyStr i :: Machine
squareReadAssemblyStr i = unlines
     $ [ "registers " ++ show i
       , "l1 : decjz r0 l2"
       , "       inc r-1"
       , "       inc r-2"
       , "       decjz r-4 l1"
       , "l2 : decjz r-1 l3"
       , "l4 :   decjz r-2 l5"
       , "         inc r0"
       , "         inc r-3"
       , "         decjz r-4 l4"
       , "l5 :   decjz r-3 l2"
       , "         inc r-2"
       , "         decjz r-4 l5"
       , "l3 : decjz r-2 end"
       , "       decjz r-4 l3"
       ]

squareNoAssembly i = GMachineAll instrs registers 0
    where
    registers = M.fromList $
        [(Register 0, i)
        ]
    instrs = M.fromList $ zip [0..]
        [ Decjz 0 4
        ,   Inc (-1)
        ,   Inc (-2)
        ,   Decjz (-4) 0 -- uncond
        , Decjz (-1) 12
        ,   Decjz (-2) 9
        ,     Inc 0
        ,     Inc (-3)
        ,     Decjz (-4) 5 -- uncond
        ,   Decjz (-3) 4
        ,     Inc (-2)
        ,     Decjz (-4) 9 -- uncond
        , Decjz (-2) 14
        ,   Decjz (-4) 12 -- uncond
        ]

readMacroAssembly i = read $ readMacroAssemblyStr i :: GMachine (Macro Instr) Integer
readMacroAssemblyStr i = unlines
     $ [ "registers " ++ show i
       , "l1 : decjz r0 l2"
       , "       inc r-1"
       , "       inc r-2"
       , "       decjz r-4 l1"
       , "l2 : decjz r-1 l3"
       , "l4 :   decjz r-2 l5"
       , "         inc r0"
       , "         inc r-3"
       , "         macro hey r-3"
       , "         decjz r-4 l4"
       , "l5 :   decjz r-3 l2"
       , "         inc r-2"
       , "         decjz r-4 l5"
       , "l3 : decjz r-2 end"
       , "       decjz r-4 l3"
       ]
