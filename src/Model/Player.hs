-------------------------------------------------------------------------------
-- This module defines a Player.
-------------------------------------------------------------------------------
module Model.Player 
(
    -- Types
    Player (..),

    -- Constants
    human
)
where

-- A player has a name.
data Player = Player {
    -- The name of the player
    plName  :: String 
} 

-- Create a player with the name human
human :: Player 
human = Player "human"
