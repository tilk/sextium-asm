module Main where

import SextiumAsmLabel
import SextiumAsmLabelParser
import SextiumAsmLabelLower
import Text.Parsec(parse)
import Text.Printf
import Data.Int

main = do
    c <- getContents
    case parse parseProg "-" c of
        Right p -> putStrLn $ unwords $ map (printf "%04X") $ map sexWordToInt16 $ lowerLProg p

