module Main where

import UI.NCurses

import Colors
import Events
import TeaNCurses
import Types
import Update
import View

main :: IO ()
main = runCurses $ do
    setEcho False
    cs <- colorIDs
    w <- newWindow 40 17 0 0
    let initState = Model { colours = cs, answer = (GcBlue, GcRed, GcWhite, GcPurple), nextGuess = [], guesses = [], gameWon = GsPlaying }
    program TeaApp { teaWindow = w, teaInit = initState, teaView = myView, teaUpdate = myUpdate, teaParseEvent = myParseEvent }

