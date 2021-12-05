-------------------------------------------------------------------------------
-- This module defines the Setup Scrabble entrypoint into the application.
-------------------------------------------------------------------------------
module SetupScrabble (
    setupScrabble
)
where

import           Brick

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import           System.Exit (exitSuccess)

-- | Sets up Scrabble
setupScrabble :: IO Int
setupScrabble = defaultMain app Nothing >>= maybe exitSuccess return

-- | Defines the application
app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

-- | Draws the UI to choose the number of players
ui :: Widget ()
ui =
  padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ vLimit 22
    $ hLimit 44
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Scrabble")
    $ C.center
    $ str " Choose Number of Players (1-4)"

-- | Setup event allows players to choose the number of players for the game.
-- Players can exit the setup event with `Esc`, `q`, or `Q`.
handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey V.KEsc        _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar d)  []))
  | d `elem` ['1'..'4'] = halt $ Just (read [d])
  | otherwise           = continue n
handleEvent n _         = continue n
