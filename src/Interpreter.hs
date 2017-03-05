{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import SextiumAsm
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Free
import Data.Int
import Data.Bits
import Data.Map(Map)
import Data.Vector(Vector,(!))
import qualified Data.Map as Map

--import Debug.Trace
trace _ a = a

type Mem = Map Int16 Int16

type Cont = State ProcState ()

data ProcState = ProcState {
    statePC :: Int16,
    stateACC :: Int16,
    stateAR :: Int16,
    stateDR :: Int16,
    stateMem :: Mem,
    stateFB :: Mem,
    stateIn :: [Int16],
    stateOut :: [Int16]
}

mlookup k m | Just v <- Map.lookup k m = v
            | otherwise = 0

interpretSexSyscall :: MonadState ProcState m => SexSyscall -> m () -> m ()
interpretSexSyscall SexSyscallHalt c = return ()
interpretSexSyscall SexSyscallRead c = modify (\s -> s { stateACC = head (stateIn s), stateIn = tail (stateIn s) }) >> c
interpretSexSyscall SexSyscallWrite c = modify (\s -> trace ("AR " ++ show (stateAR s) ++ " DR " ++ show (stateDR s)) $ s { stateOut = stateDR s : stateOut s }) >> c
interpretSexSyscall SexSyscallReadFrame c = modify (\s -> s { stateACC = mlookup (stateAR s) (stateFB s) }) >> c
interpretSexSyscall SexSyscallWriteFrame c = modify (\s -> trace ("wf " ++ show (stateAR s) ++ " " ++ show (stateDR s)) $ s { stateFB = Map.insert (stateAR s) (stateDR s) (stateFB s) }) >> c
interpretSexSyscall SexSyscallWriteIO c = c 

interpretSexInsn :: MonadState ProcState m => SexInsn -> m () -> m ()
interpretSexInsn SexInsnNop c = c
interpretSexInsn SexInsnSyscall c = do
    s <- get
    interpretSexSyscall (toEnum $ fromIntegral $ stateACC s) c 
interpretSexInsn SexInsnLoad c = modify (\s -> trace ("LOAD " ++ show (stateAR s) ++ "=" ++ show (mlookup (stateAR s) (stateMem s))) $ s { stateACC = mlookup (stateAR s) (stateMem s) }) >> c
interpretSexInsn SexInsnStore c = modify (\s -> trace ("STORE " ++ show (stateAR s) ++ " := " ++ show (stateACC s)) $ s { stateMem = Map.insert (stateAR s) (stateACC s) (stateMem s) }) >> c
interpretSexInsn SexInsnSwapa c = modify (\s -> s { stateACC = stateAR s, stateAR = stateACC s }) >> c
interpretSexInsn SexInsnSwapd c = modify (\s -> s { stateACC = stateDR s, stateDR = stateACC s }) >> c
interpretSexInsn SexInsnBranchz c = do
    s <- get
    if stateACC s == 0 then modify (\s -> s { statePC = stateAR s }) >> interpret else c
interpretSexInsn SexInsnBranchn c = do
    s <- get
    if stateACC s < 0 then modify (\s -> s { statePC = stateAR s }) >> interpret else c
interpretSexInsn SexInsnJump c = modify (\s -> s { statePC = stateACC s }) >> interpret
interpretSexInsn SexInsnConst c = modify (\s -> s { statePC = statePC s + 1, stateACC = mlookup (statePC s) (stateMem s) }) >> c
interpretSexInsn SexInsnAdd c = modify (\s -> s { stateACC = stateACC s + stateDR s }) >> c
interpretSexInsn SexInsnSub c = modify (\s -> s { stateACC = stateACC s - stateDR s }) >> c
interpretSexInsn SexInsnMul c = modify (\s -> s { stateACC = stateACC s * stateDR s }) >> c
interpretSexInsn SexInsnDiv c = modify (\s -> s { stateACC = stateACC s `div` stateDR s }) >> c
interpretSexInsn SexInsnShift c = modify (\s -> s { stateACC = stateACC s `shift` fromIntegral (stateDR s) }) >> c
interpretSexInsn SexInsnNand c = modify (\s -> s { stateACC = complement $ stateACC s .&. stateDR s }) >> c

interpret :: MonadState ProcState m => m ()
interpret = do
    s <- get
    put (s { statePC = statePC s + 1 })
    let wl = sexInsnWordToList $ sexInsnWordFromInt16 $ mlookup (statePC s) (stateMem s)
    foldr interpretSexInsn{-(\i c -> trace (show i) $ interpretSexInsn i c)-} interpret wl

type StepInt = Free (State ProcState) ()

stepInterpret :: StepInt -> ProcState -> (Maybe StepInt, ProcState)
stepInterpret (Pure x) s = (Nothing, s)
stepInterpret (Free m) s = (Just *** id) $ runState m s


