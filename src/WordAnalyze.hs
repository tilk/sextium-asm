module Main where

import SextiumAsmLabel
import SextiumAsmLabelParser
import SextiumAsmLabelLower
import Text.Parsec(parse)
import Text.Printf
import Data.Int
import Data.List
import Control.Arrow
import Control.Monad

main = do
    c <- getContents
    case parse parseProg "-" c of
        Right p -> do
            let iws = sort [sort (sexInsnWordToList iw) | SexWordInsn iw <- lowerLProg p]
            forM_ (sort $ map (length &&& head) $ group iws) print


