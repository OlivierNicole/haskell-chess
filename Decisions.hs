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

-- Returns the list of possible gains, given a starting Max node, using the
-- alpha-beta algorithm.  Only the leaves are evaluated.
maximize' :: Tree Int -> [Int]
maximize' (Node _ children@(_:_)) = mapMin $ map minimize' children
maximize' (Node n []) = [n]

-- Returns the list of possible gains, given a starting Min node, using the
-- alpha-beta algorithm.  Only the leaves are evaluated.
minimize' :: Tree Int -> [Int]
minimize' (Node _ children@(_:_)) = mapMax $ map maximize' children
minimize' (Node n []) = [n]

-- Alpha-beta-optimized version of 'map minimum'. 'mapMin' is such that :
-- 'maximum (mapMin l) == maximum (map minimum l)'. This is the only
-- guarantee on the output of 'mapMin'.
mapMin :: (Ord a) => [[a]] -> [a]
mapMin [] = []
mapMin (l:ls) = m : mapMin' m ls
   where
   m = minimum l
   mapMin' :: (Ord a) => a -> [[a]] -> [a]
   mapMin' _ [] = []
   mapMin' biggestMinSoFar (l:ls)
      | thisMin > biggestMinSoFar = thisMin : mapMin' thisMin ls
      | otherwise                 = thisMin : mapMin' biggestMinSoFar ls
      where thisMin = minGEq biggestMinSoFar l
   -- 'minGEq m l' is 'minimum l' if that minimum is greater or equal to m,
   -- otherwise it is 'm'.
   minGEq :: (Ord a) => a -> [a] -> a
   minGEq _ [x] = x
   minGEq m (x:y:xs) | x <= m    = m
                     | otherwise = minGEq m (min x y : xs)

-- Alpha-beta-optimized version of 'map maximum'. 'mapMax' is such that :
-- 'minimum (mapMax l) == minimum (map maximum l)'. This is the only
-- guarantee on the output of 'mapMax'.
mapMax :: (Ord a) => [[a]] -> [a]
mapMax [] = []
mapMax (l:ls) = m : mapMax' m ls
   where
   m = maximum l
   mapMax' :: (Ord a) => a -> [[a]] -> [a]
   mapMax' _ [] = []
   mapMax' smallestMaxSoFar (l:ls)
      | thisMax > smallestMaxSoFar = thisMax : mapMax' thisMax ls
      | otherwise                  = thisMax : mapMax' smallestMaxSoFar ls
      where thisMax = maxLEq smallestMaxSoFar l
   -- 'maxLEq m l' is 'maximum l' if that maximum is lesser or equal to m,
   -- otherwise it is 'm'.
   maxLEq :: (Ord a) => a -> [a] -> a
   maxLEq _ [x] = x
   maxLEq m (x:y:xs) | x >= m    = m
                     | otherwise = maxLEq m (max x y : xs)

-- | Evaluate the advantage of player 'col' by exploring the game tree up to a
-- given depth, assuming 'col' is to play. A depth of zero computes the static
-- advantage (i.e., no tree generation).
advantage :: Color -> Int -> ChessBoard -> Int
advantage col depth = maximum . maximize' . fmap (static col) .
                      prune depth . gameTree

-- | Select the best legal move in a given position, exploring the game tree to
-- a given depth. A depth of zero means that only the immediate possible boards
-- are generated and statically evaluated.
bestMove :: Int -> ChessBoard -> Move
bestMove depth cb = minimumBy
   (compare `on` (advantage (other $ nextMove cb) depth . doMove cb)) $
   possibleMoves cb

