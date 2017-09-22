module Events (myParseEvent) where

import UI.NCurses

import Types

myParseEvent :: Event -> Maybe Msg
myParseEvent (EventCharacter c) = 
    if c == 'b' then Just MsgBlue
    else if c == 'r' then Just MsgRed
    else if c == 'p' then Just MsgPurple
    else if c == 'g' then Just MsgGreen
    else if c == 'w' then Just MsgWhite
    else if c == 'y' then Just MsgYellow
    else Nothing
myParseEvent (EventSpecialKey k) = 
    case k of
        KeyLeftArrow -> Just MsgBack
        KeyBackspace -> Just MsgBack
        _ -> Nothing
myParseEvent _ = Nothing