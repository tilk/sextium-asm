module SextiumAsmLabelPretty where

import SextiumAsmLabel

ppInsn SexLInsnNop = "NOP"
ppInsn SexLInsnSyscall = "SYSCALL"
ppInsn (SexLInsnSyscallWith s) = "SYSCALL " ++ ppSyscall s
ppInsn SexLInsnLoad = "LOAD"
ppInsn SexLInsnStore = "STORE"
ppInsn SexLInsnSwapa = "SWAPA"
ppInsn SexLInsnSwapd = "SWAPD"
ppInsn SexLInsnBranchz = "BRANCHZ"
ppInsn SexLInsnBranchn = "BRANCHN"
ppInsn SexLInsnJump = "JUMP"
ppInsn (SexLInsnConst i) = "CONST " ++ show i
ppInsn (SexLInsnConstLabel (SexLabel l)) = "CONST " ++ l
ppInsn (SexLInsnArith a) = ppAInsn a
ppInsn (SexLInsnLabel (SexLabel l)) = l ++ ":"
ppInsn (SexLInsnData i) = "DATA " ++ show i

ppSyscall SexSyscallHalt = "HALT"
ppSyscall SexSyscallRead = "READ"
ppSyscall SexSyscallWrite = "WRITE"
ppSyscall SexSyscallReadFrame = "READFRAME"
ppSyscall SexSyscallWriteFrame = "WRITEFRAME"
ppSyscall SexSyscallReadIO = "READIO"
ppSyscall SexSyscallWriteIO = "WRITEIO"

ppAInsn SexAInsnAdd = "ADD"
ppAInsn SexAInsnSub = "SUB"
ppAInsn SexAInsnMul = "MUL"
ppAInsn SexAInsnDiv = "DIV"
ppAInsn SexAInsnShift = "SHIFT"
ppAInsn SexAInsnNand = "NAND"
