-------------------------------------------------------------------------------
-- This module defines a Player.
-- A player has a name, a rack, and a score.
-- You can initialize a player.
-------------------------------------------------------------------------------
module Model.Player 
(
    -- Types
    Player (..)
    , PlayerMap

    -- Player API
    , initPlayer
    , initPlayersMap
    , initCurrPlayerKey
    , finalizePlayerScore

    -- PlayerMap API
    , getPlayer
    , putPlayer
    , printPlayerScores
)
where

import qualified Data.Map.Strict as M
import           Model.PlayedRack
import           Model.Rack
import           Model.Score
import           Model.Tile

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

-- A Map of Ints to players
type PlayerMap = M.Map Int Player

createScoresString :: String -> Score -> String
createScoresString name score = name ++ ": " ++ show score

printPlayerScores :: PlayerMap -> String
printPlayerScores playerMap = 
    unlines (["\nFinal Scrabble Scores:"] ++ (zipWith createScoresString playerNames playerScores))
    where
        playerScores = map getPlayerScore (M.toList playerMap)
        playerNames = map getPlayerName (M.toList playerMap)
        
getPlayerName :: (Int, Player) -> String
getPlayerName player = plName (snd player)

getPlayerScore :: (Int, Player) -> Score
getPlayerScore player = plScore (snd player)

-- | Calculate the final score for a player at the end of the game. Subtracts
-- the score of any tiles left in the player's rack from their final score.
finalizePlayerScore :: Player -> Player
finalizePlayerScore (MkPlayer name rack pr sc) = MkPlayer name [] pr sc'
  where
    sc' = sc - (sum $ map getTileScore rack)

getPlayer :: PlayerMap -> Int -> Player
getPlayer playerMap = (playerMap M.!)

putPlayer :: Int -> Player -> PlayerMap -> PlayerMap
putPlayer key player playerMap = M.insert key player playerMap

-- | Initialize the current player index
initCurrPlayerKey :: Int
initCurrPlayerKey = 0

-- | Initialize a map of players
initPlayersMap :: PlayerMap
initPlayersMap = M.empty

-- | Initialize a player
initPlayer :: Int -> Rack -> Player 
initPlayer playerNum playerRack = MkPlayer 
    { plName = ("player " ++ show playerNum)
    , plRack = playerRack
    , plPlayedRack = initPlayedRack
    , plScore = initScore
    } 
