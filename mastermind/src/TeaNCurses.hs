module TeaNCurses (TeaApp(..), program)  where

import UI.NCurses

-- | a is the Model type
-- | b is the Msg type
data TeaApp a b = TeaApp 
    { teaWindow :: Window
    , teaInit :: a
    , teaView :: Window -> a -> Curses()
    , teaUpdate :: b -> a -> (a, Maybe b)
    , teaParseEvent :: Event -> Maybe b
    }

program :: TeaApp a b -> Curses()
program app = loop (teaInit app) app

loop :: a -> TeaApp a b -> Curses()
loop model app = do
    (teaView app) (teaWindow app) model
    mmsg <- waitForMsg app
    case mmsg of
        Nothing -> return ()
        (Just msg) -> do
            let (model', _) = doUpdate app model (Just msg)
            loop model' app

waitForMsg :: TeaApp a b -> Curses (Maybe b)
waitForMsg app = do
    ev <- getEvent (teaWindow app) Nothing
    case ev of
        Nothing  -> waitForMsg app
        (Just (EventSpecialKey KeyEnd)) -> return Nothing
        (Just e) -> do
            let mm = (teaParseEvent app) e
            case mm of
                Nothing  -> waitForMsg app
                (Just m) -> return (Just m)

doUpdate :: TeaApp a b -> a -> Maybe b -> (a, Maybe b)
doUpdate _   model Nothing    = (model, Nothing)
doUpdate app model (Just msg) = doUpdate app model' msg'
    where
        (model', msg') = (teaUpdate app) msg model
    
    