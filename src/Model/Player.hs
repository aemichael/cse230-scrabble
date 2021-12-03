module Model.Player where

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player { 
    plName  :: String 
} 

human :: Player 
human = Player "human"
