module Main where

import SextiumAsmLabel
import SextiumAsmLabelParser
import SextiumAsmLabelPretty
import Text.Parsec(parse)
import Data.Int
import Data.List

data RegValue = RegValueACC | RegValueAR | RegValueDR
              | RegValueConst Int16
              | RegValueLabel SexLabel
              | RegValueDeref RegValue
              | RegValueFrameDeref RegValue
              | RegValueIODeref RegValue
              | RegValueArith SexAInsn RegValue RegValue
              deriving Show

data RegState = RegState { 
    accValue :: RegValue,
    arValue :: RegValue,
    drValue :: RegValue
}

data BasicBlock = BasicBlock {
    blockLabel :: SexLabel,
    blockInitState :: RegState,
    blockInsns :: [SexLInsn]
}

initState = RegState RegValueACC RegValueAR RegValueDR

updateState :: SexLInsn -> RegState -> RegState
updateState SexLInsnNop s = s
updateState SexLInsnLoad s = s { accValue = RegValueDeref $ arValue s }
updateState SexLInsnStore s = s
updateState SexLInsnSwapa s = s { accValue = arValue s, arValue = accValue s }
updateState SexLInsnSwapd s = s { accValue = drValue s, drValue = accValue s }
updateState SexLInsnBranchz s = s
updateState SexLInsnBranchn s = s
updateState SexLInsnJump s = s
updateState (SexLInsnConst v) s = s { accValue = RegValueConst v }
updateState (SexLInsnConstLabel l) s = s { accValue = RegValueLabel l }
updateState (SexLInsnArith a) s = s { accValue = RegValueArith a (accValue s) (drValue s) }
updateState (SexLInsnData _) s = initState
updateState (SexLInsnSyscallWith SexSyscallHalt) s = initState
updateState (SexLInsnSyscallWith SexSyscallRead) s = s { accValue = RegValueACC }
updateState (SexLInsnSyscallWith SexSyscallWrite) s = s { accValue = RegValueACC }
updateState (SexLInsnSyscallWith SexSyscallReadFrame) s = s { accValue = RegValueFrameDeref $ arValue s }
updateState (SexLInsnSyscallWith SexSyscallWriteFrame) s = s { accValue = RegValueACC }
updateState (SexLInsnSyscallWith SexSyscallReadIO) s = s { accValue = RegValueIODeref $ arValue s }
updateState (SexLInsnSyscallWith SexSyscallWriteIO) s = s { accValue = RegValueACC }

blockStates :: BasicBlock -> [(SexLInsn, RegState)]
blockStates (BasicBlock l s is) = zip (SexLInsnLabel l:is) (scanl (flip updateState) s is)

progStates p = p >>= blockStates

ppAInsnOp SexAInsnAdd = "+"
ppAInsnOp SexAInsnSub = "-"
ppAInsnOp SexAInsnMul = "*"
ppAInsnOp SexAInsnDiv = "/"
ppAInsnOp SexAInsnShift = "<<"
ppAInsnOp SexAInsnNand = "~&"

ppsRegValue RegValueACC = ("ACC"++)
ppsRegValue RegValueAR = ("AR"++)
ppsRegValue RegValueDR = ("DR"++)
ppsRegValue (RegValueConst i) = (show i++)
ppsRegValue (RegValueLabel (SexLabel l)) = (l++)
ppsRegValue (RegValueDeref v) = ("*("++) . ppsRegValue v . (")"++)
ppsRegValue (RegValueFrameDeref v) = ("*F("++) . ppsRegValue v . (")"++)
ppsRegValue (RegValueArith a v1 v2) = ("("++) . ppsRegValue v1 . (' ':) . (ppAInsn a++) . (' ':) . ppsRegValue v2 . (")"++)

ppRegValue x = ppsRegValue x []

ppRegState (RegState accs ars drs) = "ACC:" ++ ppRegValue accs ++ "$AR:" ++ ppRegValue ars ++ "$DR:" ++ ppRegValue drs

ppStateLine (i, s) = ppInsn i ++ " $# " ++ ppRegState s

makeBlock (SexLInsnLabel l:is) = Just (BasicBlock l initState insns, is')
    where 
    insns = takeWhile notLabel is
    is' = dropWhile notLabel is
    notLabel (SexLInsnLabel _) = False
    notLabel _ = True
makeBlock _ = Nothing

makeBlocks = unfoldr makeBlock

main = do
    c <- getContents
    case parse parseProg "-" c of
        Right p -> putStrLn $ unlines $ map ppStateLine $ progStates $ makeBlocks p
        

