module Position (
   Position,
   fromIndex,
   toIndex,
   valid) where

-- The Position type represents a square on a chess board under the form
-- (file, row). Both components should be in [0..7].
type Position = (Int, Int)

fromIndex :: Int -> Position
fromIndex n = (n `mod` 8, n `quot` 8)

toIndex :: Position -> Int
toIndex (f, r) = 8 * r + f

valid :: Position -> Bool
valid (f, r) = valid' f && valid' r
   where valid' x = x >= 0 && x <= 7

