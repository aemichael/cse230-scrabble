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
import Model.Bonus
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
updateScore :: PlayedRack -> Board -> BonusBoard -> Score -> Score
updateScore pr b bb sc = sc + calcPlayScore pr b bb

-- | Calculate the score for the given played rack and board. We model the
-- score as being composed of \"words\", which are themselves individually
-- scored. A played word is any contiguous line (in either a row or column)
-- of at least 2 tiles, which includes at least one of the tiles in the
-- played rack. A played word must include *all* contiguous tiles in its
-- associated row or column. If a given tile is part of two played words,
-- then that tile is counted twice.
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
-- Then their score includes the played words `NEED`, `EE`, and `EX`, which
-- we calculate as (1 + 1 + 1 + 2) + (1 + 1) + (1 + 8) = 16. Observe that the
-- two `E`'s placed this turn are each counted twice (once for each word in
-- which they are included). Note also that we do *not* count the `A` or `D`
-- in `AND`, which are not part of any word formed by this play. 
calcPlayScore :: PlayedRack -> Board -> BonusBoard -> Score
calcPlayScore pr b bb = sum wordScores
  where
    scoredWordsList = S.toList $ scoredWords (map snd pr) b
    wordScores = map (calcWordScore b bb pr) scoredWordsList

-- | Get the set of played words associated with this list of played positions.
-- We model as a set rather than a list to avoid double-counting the word that
-- includes all played positions.
scoredWords :: [BoardPos] -> Board -> S.Set PlayedWord
scoredWords []     _ = S.empty
scoredWords (p:ps) b = S.unions
                        [ S.singleton $ getVertPlayedWord b p
                        , S.singleton $ getHorzPlayedWord b p
                        , scoredWords ps b
                        ]

-------------------------------------------------------------------------------
-- | PlayedWord ---------------------------------------------------------------
-------------------------------------------------------------------------------

-- | A PlayedWord is a set of board positions on which a word has been played
-- this turn. All played words must include at least one of the tiles placed on
-- the board this turn. Played words are always contiguous and in either a
-- single row or single column (they cannot include corners).
type PlayedWord = S.Set BoardPos

emptyWord :: PlayedWord
emptyWord = S.empty

isEmptyWord :: PlayedWord -> Bool
isEmptyWord word = (S.size word == 0)

-- | Insert a board position into the played word
insertPos :: PlayedWord -> BoardPos -> PlayedWord
insertPos word pos = S.union (S.singleton pos) word

-- | Combine two words into a single word, removing duplicate positions
combineWords :: PlayedWord -> PlayedWord -> PlayedWord
combineWords = S.union

-- | Calculate the score of a played word, applying any applicable bonuses
calcWordScore :: Board -> BonusBoard -> PlayedRack -> PlayedWord -> Score
calcWordScore b bb pr word = multiplier * rawScore
  where
    rawScore = sum $ map (calcTileScore b bb pr) (S.toList word)
    multiplier = getWordMultiplier bb pr word

-- | Calculate the score for a single tile of a played word, applying any
-- applicable bonuses if the tile was played on this turn.
calcTileScore :: Board -> BonusBoard -> PlayedRack -> BoardPos -> Score
calcTileScore b bb pr pos = multiplier * (getTileScore $ getTileUnsafe b pos)
  where
    multiplier = getTileBonus bb (map snd pr) pos

    getTileBonus :: BonusBoard -> [BoardPos] -> BoardPos -> Int
    getTileBonus bb ps pos | elem pos ps = case getBonus bb pos of
                                              Just DblLetter -> 2
                                              Just TrpLetter -> 3
                                              _              -> 1
                           | otherwise   = 1

-- | Calculate the full-word score multiplier of a played word. Only apply
-- multipliers activated by tiles played on this turn.
getWordMultiplier :: BonusBoard -> PlayedRack -> PlayedWord -> Int
getWordMultiplier bb pr word = product $ map (getWordBonus bb playedPos) (S.toList word)
  where
    playedPos = map snd pr

    getWordBonus :: BonusBoard -> [BoardPos] -> BoardPos -> Int
    getWordBonus bb ps pos | elem pos ps = case getBonus bb pos of
                                              Just DblWord -> 2
                                              Just TrpWord -> 3
                                              _            -> 1
                           | otherwise   = 1


-------------------------------------------------------------------------------
-- | PlayedWord Construction --------------------------------------------------
-------------------------------------------------------------------------------

-- | Get the word played from this position in the vertical (column-wise)
-- direction. If there is no such word (i.e., this tile has no contiguous
-- neighbors in its column), returns an empty set. If there is such a word,
-- returns it, including the current position.
getVertPlayedWord :: Board -> BoardPos -> PlayedWord
getVertPlayedWord b pos@(BoardPos r c) =
  if isEmptyWord upWord && isEmptyWord downWord
    then emptyWord
    else insertPos (combineWords upWord downWord) pos
  where
    upWord = getUpNeighbors b $ BoardPos (r - 1) c
    downWord = getDownNeighbors b $ BoardPos (r + 1) c

-- | Get the word played from this position in the horizontal (row-wise)
-- direction. If there is no such word (i.e., this tile has no contiguous
-- neighbors in its row), returns an empty set. If there is such a word,
-- returns it, including the current position.
getHorzPlayedWord :: Board -> BoardPos -> PlayedWord
getHorzPlayedWord b pos@(BoardPos r c) =
  if isEmptyWord leftWord && isEmptyWord rightWord
    then emptyWord
    else insertPos (combineWords leftWord rightWord) pos
  where
    leftWord = getLeftNeighbors b $ BoardPos r (c - 1)
    rightWord = getRightNeighbors b $ BoardPos r (c + 1)


-- | Get contiguous occupied positions directly up from this pos, including
-- the current position
getUpNeighbors :: Board -> BoardPos -> PlayedWord
getUpNeighbors b pos@(BoardPos r c)
  | r < 0 || not (isOccupied b pos) = emptyWord
  | otherwise                       = insertPos upNeighbors pos
  where
    upNeighbors = getUpNeighbors b $ BoardPos (r - 1) c

-- | Get contiguous occupied positions directly down from this pos, including
-- the current position
getDownNeighbors :: Board -> BoardPos -> PlayedWord
getDownNeighbors b pos@(BoardPos r c)
  | r == boardDim || not (isOccupied b pos) = emptyWord
  | otherwise                               = insertPos downNeighbors pos
  where
    downNeighbors = getDownNeighbors b $ BoardPos (r + 1) c

-- | Get contiguous occupied positions directly left of this pos, including
-- the current position
getLeftNeighbors :: Board -> BoardPos -> PlayedWord
getLeftNeighbors b pos@(BoardPos r c)
  | c < 0 || not (isOccupied b pos) = emptyWord
  | otherwise                       = insertPos leftNeighbors pos
  where
    leftNeighbors = getLeftNeighbors b $ BoardPos r (c - 1)

-- | Get contiguous occupied positions directly right of this pos, including
-- the current position
getRightNeighbors :: Board -> BoardPos -> PlayedWord
getRightNeighbors b pos@(BoardPos r c)
  | c == boardDim || not (isOccupied b pos) = emptyWord
  | otherwise                               = insertPos rightNeighbors pos
  where
    rightNeighbors = getRightNeighbors b $ BoardPos r (c + 1)
