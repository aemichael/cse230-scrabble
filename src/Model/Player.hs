-------------------------------------------------------------------------------
-- This module defines the Player class.
-------------------------------------------------------------------------------
module Model.Player 
(
    -- Types
    Player (..),

    -- Constants
    human
)
where

-- The Player data type
data Player = Player {
    -- The name of the player
    plName  :: String 
} 

-- Create a player with the name human
human :: Player 
human = Player "human"
