module ChessRules (
   legal,
   doMove,
   possibleMoves,
   gameTree) where

import ChessBoard
import Move
import Tree
import GameTree
import Color
import Position
import Data.Maybe (isNothing)
import CartesianProduct
import Data.Bifunctor (bimap)

-- | Decide whether a move is legal.
-- TODO: implement for special moves.
legal :: ChessBoard -> Move -> Bool
legal cb m@(Move dep arr) = case cb `at` dep of
   Nothing -> False
   Just (Piece col _) -> nextMove cb == col
                      && m `elem` possibleMoves cb
legal _ _ = True -- TODO


-- | Perform a move. 'doMove' does not check the validity of the move,
-- so illegal or absurd moves may be performed with this function.
-- TODO: implement for special moves.
doMove :: ChessBoard -> Move -> ChessBoard
doMove cb m@(Move dep arr) = case at cb dep of
   Nothing              -> cb
   Just p@(Piece col _) -> if col /= nextMove cb
                           then error $ "not your turn bitch!\n" ++
                             show m ++ show col ++ show (nextMove cb)
                           else switch $ remove dep $ update arr p cb
doMove cb _ = cb

-- | Generate all legal moves that may be played from a given position.
-- TODO: forbid self-checking and generate special moves.
possibleMoves :: ChessBoard -> [Move]
possibleMoves cb = concatMap ((\p -> moves p $ at cb p) . fromIndex) [0..63]
   where
   color = nextMove cb
   moves :: Position -> Maybe Piece -> [Move]
   moves pos = maybe [] (\(Piece col t) -> if col == color
                                           then moves' pos t col
                                           else [])

   moves' :: Position -> PieceType -> Color -> [Move]
   moves' pos@(f, r) Pawn color = concatMap toMove $
         filter canTake (filter valid [(f - 1, next r), (f + 1, next r)]) ++
         filter valid advance
      where
      lastRow = if color == White then 7 else 0
      sndRow  = if color == White then 1 else 6
      next    = if color == White then succ else pred
      canTake pos = case cb `at` pos of
                         Nothing -> False
                         Just (Piece col _) -> col /= color
      advance = if isNothing $ cb `at` (f, next r)
                   then (f, next r) :
                     (let j = (f, next $ next r) in
                        [j | r == sndRow && isNothing (cb `at` j)])
                   else []
      toMove arr@(f, r)
--         | next r == lastRow =
--            map (PawnPromotion (f, next r)) [Knight, Bishop, Rook,
--               Queen]
--         | otherwise = [Move pos arr]
            = [Move pos arr]

   moves' pos@(f, r) Knight color = map (Move pos) $
      filter (\p -> case cb `at` p of
                         -- only enemy pieces can be eaten
                         Just (Piece col _) -> col /= color
                         _ -> True -- arrival square is empty
             ) destinations
      where
      destinations = filter valid
         [(f + x, r + y) | (x, y) <- sq [-2,-1,1,2] ]

   moves' pos Bishop color = map (Move pos) $
      reachable cb color pos
      [bimap f g | (f, g) <- sq [pred, succ] ]

   moves' pos Rook color = map (Move pos) $
      reachable cb color pos
      [bimap id pred, bimap id succ, bimap pred id, bimap succ id]

   moves' pos Queen color =
      moves' pos Bishop color ++ moves' pos Rook color

   moves' pos King color = map (Move pos) $
      filter (\p -> case cb `at` p of
                         -- only enemy pieces can be eaten
                         Just (Piece col _) -> col /= color
                         _ -> True -- arrival square is empty
             ) destinations
      where
      destinations = filter valid
         [bimap f1 f2 pos | (f1, f2) <- sq [pred, id, succ] ]

-- Helper function
-- when given a depature position and a translation function, explore
-- yields a list of all positions it visited before it fell out of the
-- chess board, or encountered a piece of the color passed as argument,
-- or a piece of the opposite color (in that case the square of that
-- piece is included since it may be eaten).
explore :: ChessBoard -> Color -> Position -> (Position -> Position)
        -> [Position]
explore cb color dep translate = explore' dep []
   where
   explore' :: Position -> [Position] -> [Position]
   explore' p acc
      | not (valid pnext) = acc
      | otherwise = case cb `at` pnext of
                    Nothing -> explore' pnext (pnext : acc)
                    -- If obstacle of the same color, then the journey
                    -- can't go further this way
                    -- If enemy obstacle, same thing except it may be
                    -- eaten
                    Just (Piece col _) -> if col == color
                                             then acc
                                             else pnext : acc
      where pnext = translate p

-- Helper function
-- yields a list of positions reachable by 'explore' from a given
-- departure square, with a given set of translation functions.
reachable :: ChessBoard -> Color -> Position -> [Position -> Position]
          -> [Position]
reachable cb color pos = concatMap (explore cb color pos)

gameTree :: ChessBoard -> GameTree
gameTree = genTree children
   where children c = map (doMove c) (possibleMoves c)

