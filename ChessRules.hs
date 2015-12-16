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
doMove cb _ = cb -- TODO

-- | Generate all legal, regular moves starting from a given square.
localMoves :: Position -> ChessBoard -> [Move]
localMoves pos@(f,r) cb = case cb `at` pos of
   Nothing -> []
   Just (Piece col t) -> if col /= nextMove cb
      then []
      else case t of
      Pawn -> concatMap mkMove $
              filter (\p -> valid p && canTake p) [(f - 1, next r), (f + 1, next r)]
              ++ filter valid advance
         where
         canTake = maybe False (\(Piece col' _) -> col /= col') . at cb
         next = if col == White then succ else pred
         advance = if r == sndRow
                   then [p' | r' <- [next r, next (next r)]
                            , let p' = (f, r')
                            , isNothing $ cb `at` p']
                   else if isNothing $ cb `at` (f, next r)
                        then [(f, next r)]
                        else []
         mkMove arr@(f', r') = if next r == lastRow
            then [PawnPromotion (Move (f,r) (f',r')) t
                       | t <- [Knight, Bishop, Rook, Queen] ]
            else [Move pos arr]
         sndRow  = if col == White then 1 else 6
         lastRow = if col == White then 7 else 0
      Knight -> map (Move pos) [(f + x, r + y)
         | (x, y) <- sq [-2,-1,1,2]
         , valid (f+x,r+y)
         , maybe True (\(Piece col' _) -> col /= col') $ cb `at` (f+x,r+y)
         ]
      Bishop -> map (Move pos) $
         reachable cb col pos [bimap f g | (f,g) <- sq [pred, succ] ]
      Rook -> map (Move pos) $
         reachable cb col pos
         [bimap id pred, bimap id succ, bimap pred id, bimap succ id]
      Queen -> map (Move pos) $
         reachable cb col pos $
         [bimap f g | (f,g) <- sq [pred, succ] ] ++
         [bimap id pred, bimap id succ, bimap pred id, bimap succ id]
      King -> map (Move pos) $
         [p
         | (f, g) <- sq [pred, id, succ], let p = bimap f g pos
         , valid p
         , maybe True (\(Piece col' _) -> col /= col') $ cb `at` p]

-- | Generate all legal moves that may be played on a given ChessBoard.
-- TODO: forbid self-checking and generate special moves.
possibleMoves :: ChessBoard -> [Move]
possibleMoves cb = concat . chessMap (flip localMoves cb) $ cb

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

