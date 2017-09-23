module Update (myUpdate) where

import Data.List (sort)
import Types

myUpdate :: Msg -> Model -> (Model, Maybe Msg)
myUpdate MsgBack model = (model { nextGuess = safetail (nextGuess model) }, Nothing)
myUpdate msgColor model = 
    if gameWon model /= GsPlaying
    then (model, Nothing)
    else (model', Nothing)
    where
        newColor = msgToColor msgColor
        nextGuess' = newColor : nextGuess model
        (nextGuess'', guesses') = checkIfGuessDone (answer model) nextGuess' (guesses model)
        won = checkIfWon (answer model) guesses' 
        model' = model { nextGuess = nextGuess'', guesses = guesses', gameWon = won }

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

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
getResult a g = lstToTuple $ reds ++ whites ++ nones
    where
        (rs, ws) = score a g
        reds = replicate rs RcRed
        whites = replicate ws RcWhite
        nones = replicate 4 RcNone

score :: Guess -> Guess -> (Int, Int)
score at gt = (rs, ws)
  where
    as = tupleToLst at
    gs = tupleToLst gt
    xs = filter (\(x,y) -> x /= y) $ zip as gs
    rs = 4 - length xs
    (as', gs') = unzip xs
    ws = whiteScore (sort as') (sort gs')

whiteScore :: [GuessColor] -> [GuessColor] -> Int
whiteScore [] _ = 0
whiteScore _ [] = 0
whiteScore (a:as) (g:gs) = s + whiteScore as' gs'
  where
    as' = if a > g then a:as else as
    gs' = if g > a then g:gs else gs
    s = if a == g then 1 else 0

lstToTuple :: [ResultColor] -> Result
lstToTuple (a:b:c:d:_) = (a,b,c,d)
lstToTuple _ = (RcNone, RcNone, RcNone, RcNone)

tupleToLst :: Guess -> [GuessColor]
tupleToLst (a,b,c,d) = [a,b,c,d]

checkIfWon :: Guess -> [GuessLine] -> GameState
checkIfWon a gs = 
    if length gs == 0 then GsPlaying
    else if a == (fst $ head gs) then GsWon
    else if length gs > 11 then GsLost 
    else GsPlaying