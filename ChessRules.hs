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
import Control.Arrow ( (***) )

legal :: ChessBoard -> Move -> Bool
legal cb m@(Move dep arr) = case cb `at` dep of
   Nothing -> False
   Just (Piece col _) -> nextMove cb == col
                      && m `elem` possibleMoves cb
legal _ _ = True -- TODO


-- doMove does not check the validity of the move passed as its
-- argument. Illegal or absurd moves may be performed with this
-- function.
doMove :: ChessBoard -> Move -> ChessBoard
doMove cb m@(Move dep arr) = case at cb dep of
   Nothing              -> cb
   Just p@(Piece col _) -> if col /= nextMove cb
                           then error $ "not your turn bitch!\n" ++
                             show m ++ show col ++ show (nextMove cb)
                           else switch $ remove dep $ update arr p cb

doMove cb _ = cb

possibleMoves :: ChessBoard -> [Move]
possibleMoves cb = concatMap
                   (\n -> let pos = fromIndex n in
                          moves pos $ at cb pos)
                   [0..63]
   where
   color = nextMove cb
   moves :: Position -> Maybe Piece -> [Move]
   moves _ Nothing = []
   moves pos (Just (Piece col t)) = if col == color
                                       then moves' pos t color
                                       else []

   moves' :: Position -> PieceType -> Color -> [Move]
   moves' pos@(f, r) Pawn color = concatMap toMove $
         filter canTake (filter valid [(f - 1, next r), (f + 1, next r)]) ++
         filter valid advance
      where
      lastRow = if color == White then 7 else 0
      sndRow = if color == White then 1 else 6
      next = if color == White then succ else pred
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
         [(f - 2, r - 1), (f - 2, r + 1), (f - 1, r - 2), (f - 1, r + 2),
          (f + 1, r - 2), (f + 1, r + 2), (f + 2, r - 1), (f + 2, r + 1)]

   moves' pos Bishop color = map (Move pos) $
      reachable cb color pos
      [(pred, pred), (pred, succ), (succ, pred), (succ, succ)]

   moves' pos Rook color = map (Move pos) $
      reachable cb color pos
      [(id, pred), (id, succ), (pred, id), (succ, id)]

   moves' pos Queen color =
      moves' pos Bishop color ++ moves' pos Rook color

   moves' pos@(f, r) King color = map (Move pos) $
      filter (\p -> case cb `at` p of
                         -- only enemy pieces can be eaten
                         Just (Piece col _) -> col /= color
                         _ -> True -- arrival square is empty
             ) destinations
      where
      destinations = filter valid
         [(pred f, pred r), (pred f, r), (pred f, succ r),
          (f, pred r), (f, succ r),
          (succ f, pred r), (succ f, r), (succ f, succ r)]

-- Helper function
-- when given a depature position and a translation function, explore
-- yields a list of all positions it visited before it fell out of the
-- chess board, or encountered a piece of the color passed as argument, 
-- or a piece of the opposite color (in that case the square of that
-- piece is included since it may be eaten).
explore :: ChessBoard -> Color -> Position -> (Position -> Position)
   -> [Position]
explore cb color dep translation_fun = explore' dep []
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
      where pnext = translation_fun p

-- Helper function
-- yields a list of positions reachable by _explore_ from a given
-- departure square, with a given set of translation functions.
reachable :: ChessBoard -> Color -> Position ->
   [(Int -> Int, Int -> Int)] -> [Position]
reachable cb color pos = concatMap
   (\ (f, g) -> explore cb color pos (f *** g))

gameTree :: ChessBoard -> GameTree
gameTree = genTree children
   where children c = map (doMove c) (possibleMoves c)

