module Main where

import JoinList
import Scrabble (Score)
import Sized (Size)
import Buffer (fromString)
import Editor

initMsg :: JoinList (Score, Size) String
initMsg = (fromString . unlines)
       [ "This buffer is for notes you don't want to save, and for"
       , "evaluation of steam valve coefficients."
       , "To load a different file, type the character L followed"
       , "by the name of the file."
       ]

main :: IO ()
main = runEditor editor initMsg
