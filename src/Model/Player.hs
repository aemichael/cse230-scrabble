-------------------------------------------------------------------------------
-- This module defines a Player.
-------------------------------------------------------------------------------
module Model.Player 
(
    -- Types
    Player (..),

    -- Constants
    player1
)
where

-- A player has a name.
data Player = Player {
    -- The name of the player
    plName  :: String 
} 

-- Create a player with the name player1
player1 :: Player 
player1 = Player "player1"
