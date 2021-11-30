module Model.Player where

import Model.Board
import Model.Tile
import Model.Bag
import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { plName  :: String 
  , plStrat :: Strategy
  , plRack  :: Rack
  } 

type Strategy = Pos     -- ^ current cursor
             -> Board   -- ^ current board
             -> XO      -- ^ naught or cross
             -> IO Pos  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return p) []

rando :: Player 
rando = Player "machine" randomStrategy []

randomStrategy :: a -> Board -> b -> IO Pos
randomStrategy _ b _ = selectRandom (emptyPositions b) 

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

-- | A rack is an array of seven tiles. A player's rack should only have less
-- seven tiles before the game begins, or when the bag is empty.
type Rack = [TileLetter]

-- | Fill the player's rack with random tiles drawn from the bag.
fillRack :: Player -> Bag -> IO (Bag, Player)
fillRack (Player nm st rk) bag = do
  let n = min (7 - length rk) (length bag)
  (bag', rk') <- drawN n bag
  return (bag', Player nm st rk')
