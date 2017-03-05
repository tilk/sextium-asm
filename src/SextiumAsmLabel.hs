module SextiumAsmLabel(module SextiumAsm, SexLabel(..), SexLInsn(..), SexAInsn(..), SexLProg(..)) where

import Data.Int
import SextiumAsm

newtype SexLabel = SexLabel String deriving (Eq, Show, Ord)

data SexLInsn = SexLInsnNop
              | SexLInsnSyscall
              | SexLInsnSyscallWith SexSyscall
              | SexLInsnLoad
              | SexLInsnStore
              | SexLInsnSwapa
              | SexLInsnSwapd
              | SexLInsnBranchz
              | SexLInsnBranchn 
              | SexLInsnJump
              | SexLInsnConst Int16
              | SexLInsnConstLabel SexLabel
              | SexLInsnArith SexAInsn
              | SexLInsnLabel SexLabel
              | SexLInsnData Int16
    deriving (Show, Eq)

data SexAInsn = SexAInsnAdd
              | SexAInsnSub
              | SexAInsnMul
              | SexAInsnDiv
              | SexAInsnShift
              | SexAInsnNand
    deriving (Show, Eq, Enum)

type SexLProg = [SexLInsn]

