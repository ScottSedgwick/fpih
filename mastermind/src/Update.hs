module Update (myUpdate) where

import Types

myUpdate :: Msg -> Model -> (Model, Maybe Msg)
myUpdate MsgBack model = (model { nextGuess = tail (nextGuess model) }, Nothing)
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
        reds = replicate (redScore a g) RcRed
        whites = replicate (whiteScore a g) RcWhite
        nones = replicate 4 RcNone

redScore :: Guess -> Guess -> Int
redScore as gs = sum $ map f $ zip (tupleToLst as) (tupleToLst gs)
    where
        f (a, g) = if a == g then 1 else 0
        
whiteScore :: Guess -> Guess -> Int
-- can this be calculated as the sum of the min of the counts for each colour in each set?
-- nope.  It doesn't exclude the correct color & correct place ones.  Crap.
whiteScore as gs = (sum $ map tmin prs)
    where
        acs = colorCounts as
        gcs = colorCounts gs
        prs = zip acs gcs
        tmin (a,b) = if a < b then a else b

colorCounts :: Guess -> [Int]
colorCounts cs = map (colorCount cs) [GcBlue .. GcYellow]

colorCount :: Guess -> GuessColor -> Int
colorCount cs c = length $ filter (== c) (tupleToLst cs)

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