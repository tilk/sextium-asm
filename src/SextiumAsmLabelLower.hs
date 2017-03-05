module SextiumAsmLabelLower(lowerLProg) where

import SextiumAsmLabel
import SextiumAsm
import Data.Int
import Data.Map(Map)
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State

data LowerState = LowerState {
    stateWords :: [SexWord],
    stateAddr :: Int16,
    stateLabels :: Map SexLabel Int16,
    stateLabelsFinal :: Map SexLabel Int16,
    stateCurWord :: [SexInsn],
    stateCurConsts :: [Int16]
}

spillWord :: State LowerState ()
spillWord = modify $ \s -> s { 
        stateWords = map SexWordConst (stateCurConsts s) ++ SexWordInsn (sexInsnWordFromList $ reverse $ stateCurWord s):stateWords s, 
        stateAddr = stateAddr s + 1 + fromIntegral (length (stateCurConsts s)),
        stateCurWord = [],
        stateCurConsts = [] }

spillWordIfFull :: State LowerState ()
spillWordIfFull = do
    n <- gets (length . stateCurWord)
    if n == 4 then spillWord else return ()

spillWordIfNotEmpty :: State LowerState ()
spillWordIfNotEmpty = do
    n <- gets (length . stateCurWord)
    if n > 0 then spillWord else return ()

emitInsn :: SexInsn -> State LowerState ()
emitInsn i = do
    spillWordIfFull
    modify $ \s -> s { stateCurWord = i:stateCurWord s }

emitConst :: Int16 -> State LowerState ()
emitConst n = do
    spillWordIfFull
    modify $ \s -> s { stateCurWord = SexInsnConst:stateCurWord s,
                       stateCurConsts = n:stateCurConsts s }

insnOfAInsn :: SexAInsn -> SexInsn
insnOfAInsn SexAInsnAdd = SexInsnAdd
insnOfAInsn SexAInsnSub = SexInsnSub
insnOfAInsn SexAInsnMul = SexInsnMul
insnOfAInsn SexAInsnDiv = SexInsnDiv
insnOfAInsn SexAInsnShift = SexInsnShift
insnOfAInsn SexAInsnNand = SexInsnNand

insnOfLInsn :: SexLInsn -> Maybe SexInsn
insnOfLInsn SexLInsnNop = Just SexInsnNop
insnOfLInsn SexLInsnSyscall = Just SexInsnSyscall
insnOfLInsn SexLInsnLoad = Just SexInsnLoad
insnOfLInsn SexLInsnStore = Just SexInsnStore
insnOfLInsn SexLInsnSwapa = Just SexInsnSwapa
insnOfLInsn SexLInsnSwapd = Just SexInsnSwapd
insnOfLInsn SexLInsnBranchz = Just SexInsnBranchz
insnOfLInsn SexLInsnBranchn = Just SexInsnBranchn
insnOfLInsn SexLInsnJump = Just SexInsnJump
insnOfLInsn (SexLInsnArith a) = Just $ insnOfAInsn a
insnOfLInsn _ = Nothing

fromJustErr s (Just v) = v
fromJustErr s Nothing = error s

lowerLInsn :: SexLInsn -> State LowerState ()
lowerLInsn i | Just i' <- insnOfLInsn i = emitInsn i'
lowerLInsn (SexLInsnSyscallWith s) = emitConst (sexSyscallToInt16 s) >> emitInsn SexInsnSyscall
lowerLInsn (SexLInsnConst c) = emitConst c
lowerLInsn (SexLInsnConstLabel l) = 
    gets (fromJustErr (show l) . Map.lookup l . stateLabelsFinal) >>= emitConst
lowerLInsn (SexLInsnLabel l) = do
    spillWordIfNotEmpty
    modify $ \s -> s { stateLabels = Map.insert l (stateAddr s) (stateLabels s) }
lowerLInsn (SexLInsnData v) = do
    spillWordIfNotEmpty
    modify $ \s -> s { stateWords = SexWordConst v:stateWords s, 
                       stateAddr = stateAddr s + 1 }

lowerLProg :: SexLProg -> SexProg
lowerLProg p = reverse $ stateWords fs where
    is = LowerState [] 0 Map.empty (stateLabels fs) [] []
    fs = execState (mapM_ lowerLInsn p >> spillWordIfNotEmpty) is


