module CrazyhouseFormat where

-- DIESES MODUL NICHT ÄNDERN!!!!

import System.IO
import Util
import Data.List
import Control.Monad
import Test.HUnit
import qualified Data.Char as Char

import CrazyhouseBot

isRow r = r `elem` ['1'..'9']
isCol c = c `elem` ['a'..'i']
isDigit c = c `elem` ['0'..'7']

isFigure, isRank, isFile :: Char -> Bool
isFigure c = Char.toLower c `elem` ['k','q','b','n','r','p']
isRank = between '1' '8'
isFile = between 'a' 'h' 

between low high x = low <= x && x <= high

isValidMove :: String -> Bool
isValidMove [srcFig, '-', dstRank, dstFile] =
    isFigure srcFig && isRank dstRank && isFile dstFile

isValidMove [srcRank, srcFile, '-', dstRank, dstFile] =
    isRank srcRank && isFile srcFile && isRank dstRank && isFile dstFile

isValidMove _ = False


isValidListOf :: (String -> Bool) -> String -> Bool
isValidListOf validItem ('[' : str) = 
    (last str == ']') && all validItem (Util.splitOn ',' $ init str)


assertFormat :: String -> (String -> Bool) -> Assertion
assertFormat actual check =
    unless (check actual) (assertFailure msg)
    where msg = "Wrong format! Looks like: \"" ++ actual ++ "\""

--------------------------------------------------------------------------

testFormat = TestList
    [ (TestLabel "MOVE FORMAT WRONG!" (TestCase (assertFormat (CrazyhouseBot.getMove "rnbQ2Q1/pppp3p/6k1/8/1P6/8/Pn1pPKPP/RNB2BNR/BPQRppq w") isValidMove)))
    , (TestLabel "LIST FORMAT WRONG!" (TestCase (assertFormat (CrazyhouseBot.listMoves "rnbQ2Q1/pppp3p/6k1/8/1P6/8/Pn1pPKPP/RNB2BNR/BPQRppq w") $ isValidListOf isValidMove)))
    ]

main :: IO (Counts, Int)
main = runTestText (putTextToHandle stdout False) testFormat
