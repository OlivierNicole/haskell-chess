module GameTree (
   GameTree (..)
   ) where

import Tree
import ChessBoard

-- Each Node of a GameTree has zero or more child nodes, representing
-- the positions that can be reached in one move from this node.
type GameTree = Tree ChessBoard

--instance Show GameTree where
--   show (GameTreeNode []) = "[]"
--   show (GameTreeNode l) = concatMap (show' 0) l
--      where
--      -- The Int argument is the indenting level
--      show' :: Int -> (Move, GameTree) -> String
--      show' n (m, GameTreeNode l) = concat (replicate n "   ")
--         ++ show m
--         ++ "\n"
--         ++ concatMap (show' (n + 1)) l

