{-# LANGUAGE BangPatterns #-}
module ChessBoard (
     PieceType (..)
   , Piece (Piece)
   , ChessBoard
   , nextMove
   , switch
   , emptyBoard
   , initialPosition
   , at
   , update
   , remove
   , toList
   , save
   , restore
   , chessMap
   ) where

import qualified Data.Char as C
import qualified Data.Vector as V
import Data.List (intersperse, intercalate)
import Color
import Position
import Control.Monad (liftM)

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
   show (Piece White t) = map C.toUpper $ show t
   show (Piece Black t) = show t

instance Read Piece where
   readsPrec _ [c] = if lc `elem` "pnbrqk"
                     then let color = if C.isUpper c then White else Black
                              t     = case lc of
                                      'p' -> Pawn
                                      'n' -> Knight
                                      'b' -> Bishop
                                      'r' -> Rook
                                      'q' -> Queen
                                      'k' -> King
                        in [(Piece color t, "")]
                     else []
      where lc = C.toLower c

-- | Representation of a chess position.
data ChessBoard = ChessBoard
   { -- | Transforms a ChessBoard into a 'Vector' of square (either populated
     -- or empty), index 0 being square a1 and index 63 being square h8.
     toVector :: !(V.Vector (Maybe Piece))
     -- | Player making the next move.
   , nextMove :: !Color
   }

-- | Change player making the next move.
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
      showRank r = C.chr $ C.ord '1' + r
      showFile f = C.chr $ C.ord 'a' + f
      slice8 :: V.Vector a -> V.Vector (V.Vector a)
      slice8 v
         | V.null v = V.empty
         | V.length v < 8 = V.singleton v
         | otherwise = h `V.cons` slice8 t
            where (h, t) = V.splitAt 8 v

-- | An empty chess board.
emptyBoard :: Color -> ChessBoard
emptyBoard !firstPlayer = ChessBoard {
   toVector = V.replicate 64 Nothing
 , nextMove = firstPlayer
 }

-- | Classical initial position of a chess game.
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

-- | Access to a position in O(1).
at :: ChessBoard -> Position -> Maybe Piece
at !cb !p
   | i < 0 || i > 63 = Nothing
   | otherwise = toVector cb V.! i
   where i = toIndex p

-- | Put a piece on the chess board. If the position is not valid, this
-- function does nothing.
update :: Position -> Piece -> ChessBoard -> ChessBoard
update !pos !piece !cb =
   cb { toVector = toVector cb V.// [(i, Just piece)] }
   where
   i = toIndex pos

-- | Returns a ChessBoard with an empty square at the given position.
remove :: Position -> ChessBoard -> ChessBoard
remove !pos !cb
   | i < 0 || i > 63 = cb
   | otherwise       = ChessBoard (toVector cb V.// [(i, Nothing)]) $
      nextMove cb
   where
   i = toIndex pos

toList :: ChessBoard -> [Maybe Piece]
toList !cb = V.toList $ toVector cb

-- | Produce a compact string describing the chess board, e.g. for storing to a
-- file.
save :: ChessBoard -> String
save cb = map (maybe '-' (head . show)) (toList cb) ++ case nextMove cb of
                                                       White -> "W"
                                                       Black -> "B"

-- | Construct a chess board from a string produced by 'save'.
restore :: Monad m => String -> m ChessBoard
restore s
   | length s /= 65 = fail "restore: no parse (string is not 64-char long)"
   | otherwise      = ChessBoard <$>
        (V.fromList <$> mapM (readMaybePiece . (:[])) (take 64 s)) <*>
        (let c = last s in case c of
                      'W' -> return White
                      'B' -> return Black
                      _   -> fail $ "restore: no parse for last char " ++ [c]
        )
   where readMaybePiece :: Monad m => String -> m (Maybe Piece)
         readMaybePiece [c] = case reads [c] of
            [(p, "")] -> return $ Just p
            _         -> if c == '-'
                         then return Nothing
                         else fail $ "restore: no parse on char " ++ [c]
         readMaybePiece otherStr = fail $ "restore: no parse for " ++ otherStr

-- | 'chessMap f cb' maps 'f' to every position of the ChessBoard.
chessMap :: (Position -> a) -> ChessBoard -> [a]
chessMap f cb = map (f . fromIndex) [0..63]

