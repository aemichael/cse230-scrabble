-------------------------------------------------------------------------------
-- This module defines the Scrabble game.
-- A Scrabble game has a player, board, current cursor, bag, and result.
-- You can initialize a Scrabble game.
-------------------------------------------------------------------------------

module Model where 

import qualified Model.Bag    as Bag
import qualified Model.Board  as Board
import qualified Model.Bonus  as Bonus
import qualified Model.Player as Player
import           Prelude

-------------------------------------------------------------------------------
-- Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

--------------------------------------------------------------------------------
-- Scrabble -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Scrabble has a current player, map of all players, board, cursor, and bag.
data Scrabble = MkScrabble
  { scrabbleNumPlayers    :: Int                      -- ^ number of players
  , scrabbleCurrPlayerKey :: Int                      -- ^ current player index
  , scrabblePlayersMap    :: Player.PlayerMap         -- ^ current players map
  , scrabbleBoard         :: Board.Board              -- ^ current board
  , bonusBoard            :: Bonus.BonusBoard         -- ^ current bonus tiles
  , scrabblePos           :: Board.BoardPos           -- ^ current cursor
  , scrabbleBag           :: Bag.Bag                  -- ^ current bag
  }

-- | Initialize the Scrabble game state
initScrabble :: Scrabble
initScrabble = MkScrabble 
  { scrabbleNumPlayers    = 0
  , scrabbleCurrPlayerKey = Player.initCurrPlayerKey
  , scrabblePlayersMap    = Player.initPlayersMap
  , scrabbleBoard         = Board.initBoard
  , bonusBoard            = Bonus.initBonusBoard
  , scrabblePos           = head Board.boardPositions
  , scrabbleBag           = if isTest then Bag.initTestBag else Bag.initBag
  }

-- | Gets the next player key
getNextPlayerKey :: Scrabble -> Int
getNextPlayerKey scrabble = 
  if (numPlayers - 1) == currPlayerKey
  then 0
  else currPlayerKey + 1
  where
    currPlayerKey = (scrabbleCurrPlayerKey scrabble)
    numPlayers = (scrabbleNumPlayers scrabble)

-- | Checks if the coordinate (r,c) the same as the current position of the board
isCurrentPos :: Scrabble -> (Int, Int) -> Bool
isCurrentPos s (r,c) = (Board.pRow p == r) && (Board.pCol p == c)
  where 
    p = scrabblePos s 

-- | Determines what the next GameState should be based on the current boardState
next :: Scrabble -> Board.Result Board.Board -> Player.PlayerMap -> Int -> Bag.Bag -> Either Scrabble Scrabble
next s Board.Retry     _ _ _      = Right s
next s (Board.Cont b') p' k' bag' = Right $ s
  { scrabbleBoard = b'
  , scrabblePlayersMap = p'
  , scrabbleCurrPlayerKey = k'
  , scrabbleBag = bag'
  }
next s (Board.End b') p' k' bag'  = Left $ s 
  { scrabbleBoard = b'
  , scrabblePlayersMap = p'
  , scrabbleCurrPlayerKey = k'
  , scrabbleBag = bag'
  }

--------------------------------------------------------------------------------
-- Testing -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Turn this on to use testing values (e.g. a smaller initial bag)
isTest :: Bool
isTest = True
