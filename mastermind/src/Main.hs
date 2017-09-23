module Main where

import System.Random (randomIO)
import UI.NCurses

import Colors
import Events
import RandomBoundedEnum
import Types
import UI.NCurses.Tea
import Update
import View

randomColor :: IO GuessColor
randomColor = randomIO

randomGuess :: IO Guess
randomGuess = do
    a <- randomColor
    b <- randomColor
    c <- randomColor
    d <- randomColor
    return (a,b,c,d)
    
lstToTuple :: [GuessColor] -> Guess
lstToTuple (a:b:c:d:_) = (a,b,c,d)
lstToTuple _ = (GcWhite, GcWhite, GcWhite, GcWhite)

main :: IO ()
main = do
    -- Randomly generate an answer
    ans <- randomGuess
    runCurses $ do
        setEcho False
        cs <- colorIDs
        w <- newWindow 40 40 0 0
        let initState = Model { colours = cs, answer = ans, nextGuess = [], guesses = [], gameWon = GsPlaying }
        program TeaApp { teaWindow = w, teaInit = initState, teaView = myView, teaUpdate = myUpdate, teaParseEvent = myParseEvent }

