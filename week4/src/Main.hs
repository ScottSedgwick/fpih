module Main where

import ShowParser
import JSONTypes
import CaesarCipher
import Test.QuickCheck
import Data.List

data PersonRecord  = MkPersonRecord {
    name :: String,
    address :: Address,
    id :: Integer,
    labels :: [Label]    
} deriving (Show)

data Address = MkAddress {
    line1 :: String,
    number :: Integer,
    street :: String,
    town :: String,
    postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

rec1 :: PersonRecord
rec1 = MkPersonRecord 
        "Wim Vanderbauwhede" 
        (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
        557188
        [Green, Red]

rec2 :: PersonRecord
rec2 = MkPersonRecord 
        "Jeremy Singer" 
        (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
        42
        [Blue, Yellow]

rec_str :: String
rec_str = show [rec1, rec2]

test_caesar :: Int -> [Char] -> Bool
test_caesar n s = decipher n (cipher n s) == s

test_abs :: Int -> Bool
test_abs n = abs n == n || 0 - abs n ==n

test_intlist :: [Int] -> Bool
test_intlist [] = True
test_intlist ls = minimum ls == head (sort ls)

main :: IO ()
main = do
    quickCheck test_caesar
    quickCheck test_abs
    quickCheck test_intlist
