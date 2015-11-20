module Decisions (
   bestMove,
   static,
   advantage,
   ) where

-- For simplicity and performance, the game engine considers that the
-- goal is to take the opposite king, though in real chess this move is
-- forbidden. It is the engine user's (typically, the interface's)
-- responsibility to check the game state every move to ensure these
-- illegal moves do not happen.

import ChessBoard
import Color
import Move
import Tree
import GameTree
import ChessRules
import Data.List (maximumBy, minimumBy)
import Data.Function (on)

-- returns the heuristic value of some piece type. This value will be part
-- of the criteria used to evaluate a player's advantage.
-- Scores are discrete. To allow rather fine-grained evaluation,
-- the standard heuristic values are multiplied by 100.

value :: PieceType -> Int
value Pawn = 100
value Knight = 300
value Bishop = 300
value Rook = 500
value Queen = 900
value King = 40000 -- The sum of other heuristic criteria is assumed to be
                   -- smaller than 1.000

-- Returns a heuristic score representing the advantage of the 
-- side given in argument.
-- This score is roughly bounded between -80.000 and 80.000.
-- A score below -40.000 means you win, above 40.000 means you lose.
-- 0 is a draw.
static :: Color -> ChessBoard -> Int
static color cb = sum $ map signedValue $ toList cb
   where
   signedValue :: Maybe Piece -> Int
   signedValue Nothing = 0
   signedValue (Just (Piece col t)) =
      (if color == col then id else negate) $ value t

-- | Returns the maximum gain possible on a node using the Alpha-Beta algorithm.
-- Only the leaves are evaluated.
maximize :: Tree Int -> Int
maximize = maximum . maximize'

-- | Returns the minimum gain possible on a node using the Alpha-Beta algorithm.
-- Only the leaves are evaluated.
minimize :: Tree Int -> Int
minimize = minimum . minimize'

-- Returns the list of possible gains on a node using the Alpha-Beta algorithm.
-- Only the leaves are evaluated.
maximize' :: Tree Int -> [Int]
maximize' (Node _ children@(_:_)) = mapMinMax (<=) $ map minimize' children
maximize' (Node n _) = [n]

minimize' :: Tree Int -> [Int]
minimize' (Node _ children@(_:_)) = mapMinMax (>=) $ map maximize' children
minimize' (Node n []) = [n]

-- LOOKS FALSE
-- Returns the maximum among the minima of several lists, with
-- optimisations, if the function given in argument is (<=). If it is
-- (>=), then returns the minimum among the maxima of the lists.
mapMinMax :: (Ord a) => (a -> a -> Bool) -> [[a]] -> [a]
mapMinMax f (l : ls) = m : omit f m ls
   where m = minimum l

omit :: (Ord a) => (a -> a -> Bool) -> a -> [[a]] -> [a]
omit f pot [] = []
omit f pot (l : ls) =
   if minleq f l pot
      then omit f pot ls
      else m : omit f m ls
   where
   m = minimum l

minleq :: (a -> a -> Bool) -> [a] -> a -> Bool
minleq _ [] _ = False
minleq f (x : xs) pot = f x pot || minleq f xs pot

-- Evaluates the advantage of the side given in argument by exploring
-- the game tree up to a given depth. A depth of zero computes the
-- static advantage (i.e., no tree generation).
advantage :: Color -> Int -> ChessBoard -> Int
advantage col depth cb = maximize . fmap (static col) .
   prune depth . gameTree $ cb

bestMove :: Int -> ChessBoard -> Move
bestMove n cb = minimumBy
   (compare `on` (advantage (other $ nextMove cb) n . doMove cb)) $
   possibleMoves cb

