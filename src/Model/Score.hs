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
    , getScore
    , updateScore
)
where

import Prelude

-------------------------------------------------------------------------------
-- | Constants --------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------
-------------------------------------------------------------------------------

-- A score is an integer sum of all points from tiles.
type Score = Int

-- The initialize a Score
initScore :: Score
initScore = 0

-- Gets the score
getScore :: Score -> Score
getScore curr_score = curr_score

-- Updates the score
updateScore :: Int -> Score -> Score
updateScore points curr_score = points + curr_score
