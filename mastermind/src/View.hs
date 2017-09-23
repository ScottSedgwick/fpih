module View (myView) where

import UI.NCurses
import Colors
import Types

myView :: Window -> Model -> Curses()
myView w model = do
    updateWindow w $ do
        drawBoard cs
        drawGuesses cs (guesses model)
        drawPartialGuess cs (fromIntegral $ 1 + (length $ guesses model) * 2) (reverse (nextGuess model))
        drawPlayingMessage cs (gameWon model)
        drawAnswer cs (gameWon model) (answer model)
        drawInstructions cs
        moveCursor 0 0
    render
  where
    cs = colours model

drawPlayingMessage :: [ColorID] -> GameState -> Update()
drawPlayingMessage cs gs = do
    let s = "State: " ++
            case gs of
                GsPlaying -> "Playing"
                GsWon     -> "You win!"
                GsLost    -> "You lost!"
    moveCursor 1 18
    setColor $ guessColor cs GcWhite
    drawString s

drawInstructions :: [ColorID] -> Update()
drawInstructions cs = do
    setColor $ guessColor cs GcWhite
    moveCursor 3 18
    drawString "Instructions:"
    moveCursor 4 18
    drawString "============="
    moveCursor 5 18
    drawString "Colors: 'b' = Blue"
    moveCursor 6 26
    drawString "'g' = Green"
    moveCursor 7 26
    drawString "'p' = Purple"
    moveCursor 8 26
    drawString "'r' = Red"
    moveCursor 9 26
    drawString "'w' = White"
    moveCursor 10 26
    drawString "'y' = Yellow"
    moveCursor 12 18
    drawString "Left Arrow = Backspace"
    moveCursor 13 18
    drawString "End = Quit Game"

drawThing :: Glyph -> Integer -> Integer -> ColorID -> Update()
drawThing g x y c = do
    moveCursor y x
    setColor c
    drawGlyph g

drawGPeg :: Integer -> Integer -> ColorID -> Update()
drawGPeg = drawThing glyphDiamond

drawGuesses :: [ColorID] -> [GuessLine] -> Update()
drawGuesses cs gs = mapM_ (drawGuessLine cs) $ zip [1..] (reverse gs)

drawRPeg :: Integer -> Integer -> ColorID -> Update()
drawRPeg = drawThing glyphBullet

drawGuess :: [ColorID] -> Integer -> Guess -> Update()
drawGuess cs y (g1,g2,g3,g4) = do
    drawGPeg 2 y $ guessColor cs g1
    drawGPeg 4 y $ guessColor cs g2
    drawGPeg 6 y $ guessColor cs g3
    drawGPeg 8 y $ guessColor cs g4

drawGuessLine :: [ColorID] -> (Integer, GuessLine) -> Update()
drawGuessLine cs (r, (gs,(r1,r2,r3,r4))) = do
    drawGuess cs y gs
    drawRPeg 11 y $ resultColor cs r1
    drawRPeg 12 y $ resultColor cs r2
    drawRPeg 13 y $ resultColor cs r3
    drawRPeg 14 y $ resultColor cs r4
  where
    y = r * 2 - 1

drawPartialGuess :: [ColorID] -> Integer -> [GuessColor] -> Update()
drawPartialGuess cs y gs = do
    -- blank out the current guesses first, in case this refresh is as a result of a backspace
    mapM_ (\x -> drawGPeg x y black) [2,4,6,8]
    mapM_ (\(x,c) -> drawGPeg x y $ guessColor cs c) (zip [2,4,6,8] gs)
  where
    black = resultColor cs RcNone

drawLine :: ColorID -> Integer -> Update()
drawLine c y = mapM_ (\x -> drawThing glyphLineH x y c) [1..15]

drawEdgedLine :: ColorID -> Integer -> Update()
drawEdgedLine c y = do
    drawThing glyphTeeL 0 y c
    mapM_ (\x -> drawThing glyphLineH x y c) [1..15]
    drawThing glyphTeeR 16 y c

drawGuessRow :: ColorID -> Integer -> Update()
drawGuessRow c y = do
    drawThing glyphLineV 0 y c
    drawThing glyphLineV 16 y c

drawBoard :: [ColorID] -> Update()
drawBoard cs = do
    drawThing glyphCornerUL 0 0 white
    drawLine white 0
    drawThing glyphCornerUR 16 0 white
    mapM_ (drawGuessRow white) [1,3..25]
    mapM_ (drawEdgedLine white) [2,4..24]
    drawThing glyphCornerLL 0 26 white
    drawLine white 26
    drawThing glyphCornerLR 16 26 white
  where
    white = guessColor cs GcWhite

drawAnswer :: [ColorID] -> GameState -> Guess -> Update() 
drawAnswer cs won ans = 
    if won /= GsPlaying
    then do
        mapM_ (\x -> drawThing glyphBlock x 25 black) [2..8]
        drawGuess cs 25 ans
    else mapM_ (\x -> drawThing glyphBlock x 25 white) [2..8]
  where
    white = guessColor cs GcWhite
    black = resultColor cs RcNone