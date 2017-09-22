module Colors (colorIDs, guessColor, resultColor) where

import Types
import UI.NCurses

colorIDs :: Curses [ColorID]
colorIDs = mapM (\(c1,c2,i) -> newColorID c1 c2 i) colorDefs

guessColor :: [ColorID] -> GuessColor -> ColorID
guessColor cs GcBlue   = cs !! 0
guessColor cs GcRed    = cs !! 1
guessColor cs GcPurple = cs !! 2
guessColor cs GcGreen  = cs !! 3
guessColor cs GcWhite  = cs !! 4
guessColor cs GcYellow = cs !! 5

resultColor :: [ColorID] -> ResultColor -> ColorID
resultColor cs RcRed   = cs !! 1
resultColor cs RcWhite = cs !! 4
resultColor cs RcNone  = cs !! 6

colorDefs :: [(Color, Color, Integer)]
colorDefs = 
    [ (ColorBlue,    ColorBlack, 1)
    , (ColorRed,     ColorBlack, 2)
    , (ColorMagenta, ColorBlack, 3)
    , (ColorGreen,   ColorBlack, 4)
    , (ColorWhite,   ColorBlack, 5)
    , (ColorYellow,  ColorBlack, 6)
    , (ColorBlack,   ColorBlack, 7)
    ]