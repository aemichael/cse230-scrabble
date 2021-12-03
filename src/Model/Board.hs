{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    ScrabbleBoard
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 
import Model.Tile

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type ScrabbleBoard = M.Map Pos TileLetter

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: ScrabbleBoard -> Pos -> Maybe TileLetter 
board ! pos = M.lookup pos board

dim :: Int
dim = 15

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

init :: ScrabbleBoard
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Retry 
  | Cont a
  deriving (Eq, Functor, Show)

put :: ScrabbleBoard -> TileLetter -> Pos -> Result ScrabbleBoard
put board lett pos = case M.lookup pos board of 
  Just _  -> Retry
  Nothing -> result (M.insert pos lett board) 

result :: ScrabbleBoard -> Result ScrabbleBoard
result b 
  | otherwise = Cont b

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = min dim (pRow p + 1) 
  } 

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min dim (pCol p + 1) 
  } 
