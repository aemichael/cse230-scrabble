module Control where

import           Brick hiding (Result)
import qualified Brick.Types as T
import           Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Graphics.Vty as V
import           Model
import           Model.Board
import           Model.Tile

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  T.VtyEvent (V.EvKey (V.KChar 'a') _) -> nextS s =<< liftIO (playLetter (Letter 'A') s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
playLetter :: TileLetter -> PlayState -> IO (Result ScrabbleBoard)
-------------------------------------------------------------------------------
playLetter lett s = put (psBoard s) lett <$> getPos s

getPos :: PlayState -> IO Pos
getPos s = do {return (psPos s)}

-------------------------------------------------------------------------------
nextS :: PlayState -> Result ScrabbleBoard -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 
