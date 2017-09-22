module Types where

import UI.NCurses

data GuessColor = GcBlue | GcRed | GcPurple | GcGreen | GcWhite | GcYellow deriving (Eq, Ord, Enum)
data ResultColor = RcRed | RcWhite | RcNone deriving (Eq, Ord, Enum)

type Guess = (GuessColor, GuessColor, GuessColor, GuessColor)
type Result = (ResultColor, ResultColor, ResultColor, ResultColor)
type GuessLine = (Guess, Result)

data Msg = MsgBlue
         | MsgRed
         | MsgPurple
         | MsgGreen
         | MsgWhite
         | MsgYellow 
         | MsgBack 
         deriving (Show, Eq, Ord)
         
msgToColor :: Msg -> GuessColor
msgToColor MsgBlue   = GcBlue
msgToColor MsgRed    = GcRed
msgToColor MsgPurple = GcPurple
msgToColor MsgGreen  = GcGreen
msgToColor MsgWhite  = GcWhite
msgToColor MsgYellow = GcYellow
msgToColor _         = GcWhite

data GameState = GsPlaying | GsWon | GsLost deriving (Show, Eq)

data Model = Model
    { colours :: [ColorID]
    , answer :: Guess
    , nextGuess :: [GuessColor]
    , guesses :: [GuessLine]
    , gameWon :: GameState
    }
    