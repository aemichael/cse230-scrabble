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
import Model.PlayedRack
import Model.Board
import qualified Data.Set as S

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

-- | Gets the score
updateScore :: PlayedRack -> Board -> Score -> Score
updateScore pr b sc = sc + calcNewScore pr b

calcNewScore :: PlayedRack -> Board -> Score
calcNewScore pr b = sum $ map getTileScore scoredTiles
  where
    scoredTiles = map (getTileUnsafe b) $ calcScoredPositions pr b

-- | Calculate the positions that should be scored for the given played rack.
-- This includes all played positions, and any adjacent positions that are
-- occupied with a tile (\"neighbors\"). When a played tile has neighbors,
-- that tile is counted twice.
-- 
-- Example: If the current board is
-- ```
--      |   | A | N | D |
--      |---|---|---|---|
--      |   |   |   | E |
--      |---|---|---|---|
--      |   |   |   | X |
--      |---|---|---|---|
--      |   |   |   |   |
-- ```
-- And the player plays `E`, `E`, `D` as so:
-- ```
--      |   | A | N | D |
--      |---|---|---|---|
--      |   |   | E | E |
--      |---|---|---|---|
--      |   |   | E | X |
--      |---|---|---|---|
--      |   |   | D |   |
-- ```
-- Then their score includes the three tiles they played, *and also* the tiles
-- `N` (first letter of `NEED`); `E` (second of `EE`); and `X` (second of `EX`).
-- In addition, the tiles `E`, `E` played on this turn are each counted twice
-- (once in each direction). You can think of this as scoring up each \"word\"
-- formed by this play: `NEED`, `EE`, and `EX`. So the final score is
-- (1 + 1 + 1 + 2) + (1 + 1) + (1 + 8) = 16.
calcScoredPositions :: PlayedRack -> Board -> [BoardPos]
calcScoredPositions pr b = playedPos ++ (S.toList $ scoredPosSet playedPos b)
  where
    playedPos = map snd pr

    scoredPosSet :: [BoardPos] -> Board -> S.Set BoardPos
    scoredPosSet []     _ = S.empty
    scoredPosSet (p:ps) b = S.union (getUnplayedNeighbors b p) (scoredPosSet ps b)

    -- Unplayed neighbors = (all neighbors) - (played positions - current pos)
    getUnplayedNeighbors :: Board -> BoardPos -> S.Set BoardPos
    getUnplayedNeighbors b p = S.difference (getAllNeighbors b p) $
                                 S.difference (S.fromList playedPos) (S.fromList [p])
