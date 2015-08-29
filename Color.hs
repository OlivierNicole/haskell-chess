module Color (
   Color (Black, White),
   other) where

data Color = Black | White
   deriving (Show, Eq)

other :: Color -> Color
other White = Black
other Black = White

