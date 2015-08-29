module MoveTree (
   MoveTree (MoveTreeNode)
   ) where

import Move

-- Each Node of a MoveTree has zero or more child nodes, along with the
-- moves to go from the parent Node to the child Node.
data MoveTree = MoveTreeNode [(Move, MoveTree)]

instance Show MoveTree where
   show (MoveTreeNode []) = "[]"
   show (MoveTreeNode l) = concatMap (show' 0) l
      where
      -- The Int argument is the indenting level
      show' :: Int -> (Move, MoveTree) -> String
      show' n (m, MoveTreeNode l) = concat (replicate n "   ")
         ++ show m
         ++ "\n"
         ++ concatMap (show' (n + 1)) l

