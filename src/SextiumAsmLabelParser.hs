module SextiumAsmLabelParser where

import SextiumAsmLabel
import Text.Parsec
import Control.Monad
import Data.Int

type P = Parsec String ()

xspace = oneOf "\t "
xspaces = many xspace

parseComment :: P ()
parseComment = try $ xspaces >> ((endOfLine >> return ()) <|> (char '#' >> manyTill anyChar endOfLine >> return ()))

insn :: String -> a -> P a
insn s i = try $ do
    x <- many1 upper
    guard (x == s)
    return i

num :: P Int16
num = read <$> (((:) <$> char '-' <*> many1 digit) <|> ((++) <$> try (string "0x") <*> many1 hexDigit) <|> many1 digit)

slabel :: P SexLabel
slabel = SexLabel <$> ((:) <$> lower <*> many alphaNum)

ilabel :: P SexLInsn
ilabel = do
    l <- slabel
    char ':'
    return $ SexLInsnLabel l

parseSyscall = insn "HALT" SexSyscallHalt
           <|> insn "READ" SexSyscallRead
           <|> insn "WRITE" SexSyscallWrite
           <|> insn "READFRAME" SexSyscallReadFrame
           <|> insn "WRITEFRAME" SexSyscallWriteFrame
           <|> insn "READIO" SexSyscallReadIO
           <|> insn "WRITEIO" SexSyscallWriteIO

parseInsn = insn "NOP" SexLInsnNop
        <|> try (insn "SYSCALL" SexLInsnSyscallWith <*> (xspaces >> parseSyscall))
        <|> insn "SYSCALL" SexLInsnSyscall
        <|> insn "LOAD" SexLInsnLoad
        <|> insn "STORE" SexLInsnStore
        <|> insn "SWAPA" SexLInsnSwapa
        <|> insn "SWAPD" SexLInsnSwapd
        <|> insn "BRANCHZ" SexLInsnBranchz
        <|> insn "BRANCHN" SexLInsnBranchn
        <|> insn "JUMP" SexLInsnJump
        <|> insn "DATA" SexLInsnData <*> (xspaces >> num)
        <|> try (insn "CONST" SexLInsnConst <*> (xspaces >> num))
        <|> insn "CONST" SexLInsnConstLabel <*> (xspaces >> slabel)
        <|> insn "ADD" (SexLInsnArith SexAInsnAdd)
        <|> insn "SUB" (SexLInsnArith SexAInsnSub)
        <|> insn "MUL" (SexLInsnArith SexAInsnMul)
        <|> insn "DIV" (SexLInsnArith SexAInsnDiv)
        <|> insn "SHIFT" (SexLInsnArith SexAInsnShift)
        <|> insn "NAND" (SexLInsnArith SexAInsnNand)
        <|> ilabel

parseLine = try $ do
    xspaces
    i <- parseInsn
    parseComment
    return i

parseProg :: P SexLProg
parseProg = skipMany parseComment >> many (parseLine <* skipMany parseComment) <* eof

