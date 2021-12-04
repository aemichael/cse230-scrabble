-------------------------------------------------------------------------------
-- This module defines a Player.
-- A player has a name, a rack, and a score.
-- You can initialize a player.
-------------------------------------------------------------------------------
module Model.Player 
(
    -- Types
    Player (..),

    -- Player API
    initPlayer
)
where

import Model.PlayedRack
import Model.Rack
import Model.Score

-- A player has a name.
data Player = MkPlayer 
    -- The name of the player
    { plName  :: String 
    -- The rack for the player
    , plRack :: Rack
    -- The rack to store tiles played in the current turn
    , plPlayedRack :: PlayedRack
    -- The score for the player
    , plScore :: Score
    } 

-- Initialize a player
initPlayer :: Player 
initPlayer = MkPlayer 
    { plName = "player" 
    , plRack = initRack
    , plPlayedRack = initPlayedRack
    , plScore = initScore
    } 
