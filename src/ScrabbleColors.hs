-------------------------------------------------------------------------------
-- This module defines background colors used to render the Scrabble game.
-------------------------------------------------------------------------------
module ScrabbleColors where

import Graphics.Vty.Attributes.Color
import Model.Bonus (Bonus (..))

-- | Default background color
defbg :: Color
defbg = rgbColor 224 204 173

-- | Background color for placed tiles
tilebg :: Color
tilebg = rgbColor 191 152 130

-- | Background color for bonus tiles
bonusbg :: Bonus -> Color
bonusbg DblLetter = rgbColor 163 186 199
bonusbg TrpLetter = rgbColor 49 136 173
bonusbg DblWord   = rgbColor 245 180 185
bonusbg TrpWord   = rgbColor 196 86 86
bonusbg Star      = bonusbg DblWord
