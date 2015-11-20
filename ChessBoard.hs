{-# LANGUAGE BangPatterns #-}
module ChessBoard (
   PieceType (..),
   Piece (Piece),
   ChessBoard,
   nextMove,
   switch,
   emptyBoard,
   initialPosition,
   at,
   update,
   remove,
   toList) where

import qualified Data.Char
import qualified Data.Vector as V
import Data.List (intersperse, intercalate)
import Color
import Position

data PieceType =
             Pawn
           | Knight
           | Bishop
           | Rook
           | Queen
           | King
   deriving (Eq)

instance Show PieceType where
   show Pawn   = "p"
   show Knight = "n"
   show Bishop = "b"
   show Rook   = "r"
   show Queen  = "q"
   show King   = "k"

data Piece = Piece !Color !PieceType
   deriving Eq

instance Show Piece where
   show (Piece White t) = map Data.Char.toUpper $ show t
   show (Piece Black t) = show t

-- The ChessBoard type represents a chess board under the form of a
-- Vector (Maybe Piece).
data ChessBoard = ChessBoard { toVector :: !(V.Vector (Maybe Piece)),
                               nextMove :: !Color }

switch :: ChessBoard -> ChessBoard
switch !cb = ChessBoard { toVector = toVector cb,
                          nextMove = other $ nextMove cb }

instance Show ChessBoard where
   -- Slice the ChessBoard into a list of rows, reverse the rows (because by
   -- convention, chess boards are shown with the white camp downside)
   -- and turns each row to a String before merging these Strings. The
   -- removeLast is here only to get rid of the extra newline added by
   -- unlines.
   show cb = (unlines . V.toList . V.reverse . V.imap showLine . slice8 . toVector) cb
      ++ "  " ++ concat (replicate 8 "+---") ++ "+\n    "
      ++ intercalate "   " (map ((:[]) . showFile) [0..7]) ++ " " ++
         show (nextMove cb)
      where
      showLine :: Int -> V.Vector (Maybe Piece) -> String
      showLine rank v =
         "  " ++ concat (replicate 8 "+---") ++ "+\n" ++ (
         showRank rank : " | " ++
         (intercalate " | " . map (:[])) (V.toList (V.imap (showSquare rank) v))
         ++ " |")
      showSquare :: Int -> Int -> Maybe Piece -> Char
      showSquare rank file Nothing = if rank `mod` 2 == file `mod` 2
                                        then '-'
                                        else ' '
      showSquare _ _ (Just p) = head $ show p
      showRank :: Int -> Char
      showRank r = Data.Char.chr $ Data.Char.ord '1' + r
      showFile f = Data.Char.chr $ Data.Char.ord 'a' + f
      slice8 :: V.Vector a -> V.Vector (V.Vector a)
      slice8 v
         | V.null v = V.empty
         | V.length v < 8 = V.singleton v
         | otherwise = h `V.cons` slice8 t
            where (h, t) = V.splitAt 8 v

emptyBoard :: Color -> ChessBoard
emptyBoard !firstPlayer = ChessBoard {
   toVector = V.replicate 64 Nothing
 , nextMove = firstPlayer
 }

initialPosition :: ChessBoard
initialPosition = ChessBoard {
   toVector = V.fromList $ concat $
   [whiteRearRow, whiteFrontRow]
   ++
   replicate 4 emptyRow
   ++
   [blackFrontRow, blackRearRow]
   , nextMove = White
   }
   where
   whiteRearRow  = map (Just . Piece White) rearRow
   whiteFrontRow = replicate 8 $ Just $ Piece White Pawn
   emptyRow      = replicate 8 Nothing
   blackFrontRow = replicate 8 $ Just $ Piece Black Pawn
   blackRearRow  = map (Just . Piece Black) rearRow
   rearRow       = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

at :: ChessBoard -> Position -> Maybe Piece
at !cb !p
   | i < 0 || i > 63 = Nothing
   | otherwise = toVector cb V.! i
   where i = toIndex p

-- Yields a ChessBoard with a new piece inserted at the given
-- position. If the position is not valid, this function does nothing.
update :: Position -> Piece -> ChessBoard -> ChessBoard
update !pos !piece !cb = ChessBoard (toVector cb V.// [(i, Just piece)])
   $ nextMove cb
   where
   i = toIndex pos

-- Returns a ChessBoard with an empty square at the given position.
remove :: Position -> ChessBoard -> ChessBoard
remove !pos !cb
   | i < 0 || i > 63 = cb
   | otherwise       = ChessBoard (toVector cb V.// [(i, Nothing)]) $
      nextMove cb
   where
   i = toIndex pos

toList :: ChessBoard -> [Maybe Piece]
toList !cb = V.toList $ toVector cb

