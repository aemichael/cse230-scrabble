-------------------------------------------------------------------------------
-- This module defines a Player.
-- A player has a name, a rack, and a score.
-------------------------------------------------------------------------------
module Model.Player 
(
    -- Types
    Player (..),

    -- Constants
    player1
)
where

import Model.Rack

-- A player has a name.
data Player = MkPlayer 
    -- The name of the player
    { plName  :: String 
    -- The rack for the player
    , plRack :: Rack
    } 

-- Create a player with the name player1
player1 :: Player 
player1 = MkPlayer 
    { plName = "player1" 
    , plRack = initRack
    } 
