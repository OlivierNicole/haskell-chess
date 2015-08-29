module Move (
   Move (..) ) where

import ChessBoard
import Color
import Position
import Data.Char (ord, chr)

-- a move can either be defined by a departure position and an arrival
-- position, or be a special move.
data Move = Move Position Position
          | KingSideCastling Color
          | QueenSideCastling Color
          -- a pawn promotion is defined by the arrival position of the
          -- pawn and the piece type it is promoted to.
          | PawnPromotion Position PieceType
   deriving (Eq)

instance Show Move where
   show (Move (f, r) (f', r')) =
      [fromFile f, fromRank r, fromFile f', fromRank r']
      where
      fromFile :: Int -> Char
      fromFile f = chr $ f + ord 'a'
      fromRank r = chr $ r + ord '1'
   show (KingSideCastling c) = "kingside castling by " ++ show c
   show (QueenSideCastling c) = "queenside castling by " ++ show c
   show (PawnPromotion (f, r) t) = show (Move (f, r-1) (f, r)) ++
      ": pawn promoted to " ++ show t

instance Read Move where
   readsPrec _ (f : r : f' : r' : tail) =
      if valid pos && valid pos'
         then (Move pos pos', tail) : reads tail
         else []
      where
      pos = toPosition f r
      pos' = toPosition f' r'
      toPosition x y = (ord x - ord 'a', ord y - ord '1')
   readsPrec _ _ = []

