-------------------------------------------------------------------------------
-- This module defines a Score.
-- A score has a sum of points.
-- You can initialize a score.
-- You can update to a score.
-- You can get a score.
-------------------------------------------------------------------------------
module Model.Score
( 
    -- Types
    Score

    -- Score API
    , initScore
    , updateScore
)
where

import Prelude
import Model.Tile

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------
-------------------------------------------------------------------------------

-- | A score is an integer sum of all points from tiles.
type Score = Int

-- | Initialize a Score
initScore :: Score
initScore = 0

-- | Updates the score
updateScore :: [Tile] -> Score -> Score
updateScore tiles score = (sum $ map getTileScore tiles) + score
