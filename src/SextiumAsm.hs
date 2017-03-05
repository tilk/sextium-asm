module SextiumAsm where

import Data.Int
import Data.Bits

data SexInsn = SexInsnNop
             | SexInsnSyscall
             | SexInsnLoad 
             | SexInsnStore
             | SexInsnSwapa
             | SexInsnSwapd
             | SexInsnBranchz
             | SexInsnBranchn
             | SexInsnJump
             | SexInsnConst
             | SexInsnAdd
             | SexInsnSub
             | SexInsnMul
             | SexInsnDiv
             | SexInsnShift
             | SexInsnNand
    deriving (Show, Eq, Enum, Ord)

data SexSyscall = SexSyscallHalt
                | SexSyscallRead
                | SexSyscallWrite
                | SexSyscallReadFrame
                | SexSyscallWriteFrame
                | SexSyscallReadIO
                | SexSyscallWriteIO
    deriving (Show, Eq, Enum, Ord)

data SexInsnWord = SexInsnWord SexInsn SexInsn SexInsn SexInsn
    deriving (Show, Eq)

data SexWord = SexWordInsn SexInsnWord
             | SexWordConst Int16
    deriving (Show, Eq)

type SexProg = [SexWord]

sexInsnWordFromList :: [SexInsn] -> SexInsnWord
sexInsnWordFromList [] = SexInsnWord SexInsnNop SexInsnNop SexInsnNop SexInsnNop
sexInsnWordFromList [i1] = SexInsnWord i1 SexInsnNop SexInsnNop SexInsnNop
sexInsnWordFromList [i1,i2] = SexInsnWord i1 i2 SexInsnNop SexInsnNop
sexInsnWordFromList [i1,i2,i3] = SexInsnWord i1 i2 i3 SexInsnNop
sexInsnWordFromList [i1,i2,i3,i4] = SexInsnWord i1 i2 i3 i4

sexInsnWordToList :: SexInsnWord -> [SexInsn]
sexInsnWordToList (SexInsnWord i1 i2 i3 i4) = [i1,i2,i3,i4]

sexSyscallToInt16 :: SexSyscall -> Int16
sexSyscallToInt16 = fromIntegral . fromEnum

sexWordToInt16 :: SexWord -> Int16
sexWordToInt16 (SexWordConst i) = i
sexWordToInt16 (SexWordInsn (SexInsnWord i1 i2 i3 i4)) = 
    fromIntegral $ (fromEnum i1 `shiftL` 12) .|. (fromEnum i2 `shiftL` 8) .|. (fromEnum i3 `shiftL` 4) .|. fromEnum i4

sexInsnWordFromInt16 :: Int16 -> SexInsnWord
sexInsnWordFromInt16 i = SexInsnWord (toEnum $ fromIntegral $ (i `shiftR` 12) .&. 0xf) 
                                     (toEnum $ fromIntegral $ (i `shiftR` 8) .&. 0xf)
                                     (toEnum $ fromIntegral $ (i `shiftR` 4) .&. 0xf)
                                     (toEnum $ fromIntegral $ i .&. 0xf)

