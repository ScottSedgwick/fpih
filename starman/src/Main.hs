module Main where

import System.IO

check :: String -> String -> Char -> (Bool, String)
check word display c = (r, d)
    where
        r = c `elem` word
        d = [if x==c then c else y | (x,y) <- zip word display]

wordout :: String -> Int -> String
wordout w n = "[" ++ show n ++ "]" ++ w

turn :: String -> String -> Int -> IO()
turn _ _ 0 = putStrLn "You lose"
turn w d n = if w==d then putStrLn (wordout w n ++ "\nYou win, with " ++ show n ++ " guesses remaining.") else mkguess w d n

mkguess :: String -> String -> Int -> IO()
mkguess w d n = do
    putStr (wordout d n ++ "  " ++ "Enter your guess: ")
    hFlush stdout
    q <- getLine
    let (c, d') = check w d (head q)
    let n' = if c then n else n - 1
    turn w d' n'

starman :: String -> Int -> IO()
starman word = turn word (map (\_ -> '-') word)

main :: IO ()
main = starman "functionally" 5
