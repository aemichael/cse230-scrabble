-------------------------------------------------------------------------------
-- This module defines Scrabble bonuses and the BonusBoard.
-- A Bonus can double or triple the score of a single letter, or the score
-- of a whole word.
-- The BonusBoard maps board positions to their associated bonuses.
-------------------------------------------------------------------------------
module Model.Bonus 
(
  -- Types
  Bonus (..)
, BonusBoard

  -- BonusBoard API
, initBonusBoard
, getBonus
)
  where

import qualified Data.Map as M
import           Prelude
import           Model.Board

-------------------------------------------------------------------------------
-- | Bonus --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | A Bonus is a score multiplier that can be applied to either a single
-- letter or a whole word. Bonuses are associated with positions on the board.
data Bonus = DblLetter
           | TrpLetter
           | DblWord
           | TrpWord
           | Star

instance Show Bonus where
  show DblLetter = "2xL"
  show TrpLetter = "3xL"
  show DblWord   = "2xW"
  show TrpWord   = "3xW"
  show Star      = "  ★  \n★ ★ ★\n  ★  "

-- | A BonusBoard defines which board positions have associated bonuses, as
-- well as the type of those bonuses.
type BonusBoard = M.Map BoardPos Bonus

-- | Retrieve the bonus associated with the given board position, if one exists
getBonus :: BonusBoard -> BoardPos -> Maybe Bonus
getBonus bb pos = M.lookup pos bb

-- | Initialize bonus board with standard Scrabble bonus positions
initBonusBoard :: BonusBoard
initBonusBoard = M.fromList
  [ (BoardPos 1 1, TrpWord)    , (BoardPos 1 4, DblLetter)  , (BoardPos 1 8, TrpWord)     , (BoardPos 1 12, DblLetter)  , (BoardPos 1 15, TrpWord)
  , (BoardPos 2 2, DblWord)    , (BoardPos 2 6, TrpLetter)  , (BoardPos 2 10, TrpLetter)  , (BoardPos 2 14, DblWord)
  , (BoardPos 3 3, DblWord)    , (BoardPos 3 7, DblLetter)  , (BoardPos 3 9, DblLetter)   , (BoardPos 3 13, DblWord)
  , (BoardPos 4 1, DblLetter)  , (BoardPos 4 4, DblWord)    , (BoardPos 4 8, DblLetter)   , (BoardPos 4 12, DblWord)    , (BoardPos 4 15, DblLetter)
  , (BoardPos 5 5, DblWord)    , (BoardPos 5 11, DblWord)  
  , (BoardPos 6 2, TrpLetter)  , (BoardPos 6 6, TrpLetter)  , (BoardPos 6 10, TrpLetter)  , (BoardPos 6 14, TrpLetter)
  , (BoardPos 7 3, DblLetter)  , (BoardPos 7 7, DblLetter)  , (BoardPos 7 9, DblLetter)   , (BoardPos 7 13, DblLetter)
  , (BoardPos 8 1, TrpWord)    , (BoardPos 8 4, DblLetter)  , (BoardPos 8 8, Star)        , (BoardPos 8 12, DblLetter)  , (BoardPos 8 15, TrpWord)
  , (BoardPos 9 3, DblLetter)  , (BoardPos 9 7, DblLetter)  , (BoardPos 9 9, DblLetter)   , (BoardPos 9 13, DblLetter)
  , (BoardPos 10 2, TrpLetter) , (BoardPos 10 6, TrpLetter) , (BoardPos 10 10, TrpLetter) , (BoardPos 10 14, TrpLetter)
  , (BoardPos 11 5, DblWord)   , (BoardPos 11 11, DblWord)
  , (BoardPos 12 1, DblLetter) , (BoardPos 12 4, DblWord)   , (BoardPos 12 8, DblLetter)  , (BoardPos 12 12, DblWord)   , (BoardPos 12 15, DblLetter)
  , (BoardPos 13 3, DblWord)   , (BoardPos 13 7, DblLetter) , (BoardPos 13 9, DblLetter)  , (BoardPos 13 13, DblWord)
  , (BoardPos 14 2, DblWord)   , (BoardPos 14 6, TrpLetter) , (BoardPos 14 10, TrpLetter) , (BoardPos 14 14, DblWord)
  , (BoardPos 15 1, TrpWord)   , (BoardPos 15 4, DblLetter) , (BoardPos 15 8, TrpWord)    , (BoardPos 15 12, DblLetter) , (BoardPos 15 15, TrpWord)
  ]
