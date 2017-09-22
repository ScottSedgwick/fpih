module Update (myUpdate) where

import Types

myUpdate :: Msg -> Model -> (Model, Maybe Msg)
myUpdate MsgBack model = (model { nextGuess = tail (nextGuess model) }, Nothing)
myUpdate msgColor model = (model', Nothing)
    where
        newColor = msgToColor msgColor
        nextGuess' = newColor : nextGuess model
        (nextGuess'', guesses') = checkIfGuessDone (answer model) nextGuess' (guesses model)
        won = checkIfWon (answer model) guesses' 
        model' = model { nextGuess = nextGuess'', guesses = guesses', gameWon = won }

checkIfGuessDone :: Guess -> [GuessColor] -> [GuessLine] -> ([GuessColor], [GuessLine])
checkIfGuessDone ans ng gs =
    if length ng < 4
    then (ng, gs)
    else ([], (colorsToGuess ans ng) : gs)

colorsToGuess :: Guess -> [GuessColor] -> GuessLine
colorsToGuess ans (a:b:c:d:_) = (guess, result)
    where
        guess = (d,c,b,a)
        result = getResult ans guess
colorsToGuess _ _ = ((GcWhite, GcWhite, GcWhite, GcWhite), (RcNone, RcNone, RcNone, RcNone)) -- this should never happen because I check for length above

getResult :: Guess -> Guess -> Result
getResult _ _ = (RcNone, RcNone, RcNone, RcNone)

checkIfWon :: Guess -> [GuessLine] -> GameState
checkIfWon a gs = 
    if length gs == 0 then GsPlaying
    else if a == (fst $ head gs) then GsWon
    else if length gs > 11 then GsLost 
    else GsPlaying