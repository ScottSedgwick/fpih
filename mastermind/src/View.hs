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
        moveCursor 0 0
    render
  where
    cs = colours model

drawThing :: Glyph -> Integer -> Integer -> ColorID -> Update()
drawThing g x y c = do
    moveCursor y x
    setColor c
    drawGlyph g

drawGPeg :: Integer -> Integer -> ColorID -> Update()
drawGPeg = drawThing glyphDiamond

drawGuesses :: [ColorID] -> [GuessLine] -> Update()
drawGuesses cs gs = mapM_ (drawGuessLine cs) $ zip [2,4..] gs

drawRPeg :: Integer -> Integer -> ColorID -> Update()
drawRPeg = drawThing glyphBullet

drawGuessLine :: [ColorID] -> (Integer, GuessLine) -> Update()
drawGuessLine cs (r, ((g1,g2,g3,g4),(r1,r2,r3,r4))) = do
    drawGPeg 2 y $ guessColor cs g1
    drawGPeg 4 y $ guessColor cs g2
    drawGPeg 6 y $ guessColor cs g3
    drawGPeg 8 y $ guessColor cs g4
    drawRPeg 11 y $ resultColor cs r1
    drawRPeg 12 y $ resultColor cs r2
    drawRPeg 13 y $ resultColor cs r3
    drawRPeg 14 y $ resultColor cs r4
  where
    y = r * 2 - 1

drawPartialGuess :: [ColorID] -> Integer -> [GuessColor] -> Update()
drawPartialGuess cs y gs = mapM_ (\(x,c) -> drawGPeg x y $ guessColor cs c) (zip [2,4,6,8] gs)

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
    mapM_ (drawGuessRow white) [1,3..23]
    mapM_ (drawEdgedLine white) [2,4..24]
  where
    white = guessColor cs GcWhite

-- drawAnswer :: Window -> [ColorID] -> Guess -> Bool -> Curses()
-- drawAnswer w cs answer shw =
--     if shw 
--     then drawRealAnswer w cs answer
--     else drawHiddenAnswer w cs

-- drawRealAnswer :: Window -> [ColorID] -> Guess -> Curses()
-- drawRealAnswer = undefined

-- drawHiddenAnswer :: Window -> [ColorID] -> Curses()
-- drawHiddenAnswer w cs = do
--     let c = guessColor cs GcWhite
--     updateWindow w $ mapM_ (\x -> drawThing glyphBlock x 25 c) [2..8]
--     render