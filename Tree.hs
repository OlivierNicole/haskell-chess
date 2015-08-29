module Tree (
   Tree (Node),
   genTree,
   prune,
   takeTree
   ) where

data Tree a = Node a [Tree a]

instance Functor Tree where
   fmap f (Node label children) =
      Node (f label) (map (fmap f) children)

instance (Show a) => Show (Tree a) where
   show = show' 0
      where
      -- n est le niveau d'indentation
      show' n (Node a x) = replicate n '*' ++ show a ++ "\n"
         ++ concatMap (show' (n + 1)) x

-- builds a tree by repeated applications of a function
genTree :: (a -> [a]) -> a -> Tree a
genTree f a = Node a $ map (genTree f) $ f a

-- prunes a tree to a fixed depth
prune :: Int -> Tree a -> Tree a
prune 0 (Node a _) = Node a []
prune n (Node a children) = Node a $ map (prune $ n - 1) children

-- Recursively removes all nodes but the first n ones
takeTree :: Int -> Tree a -> Tree a
takeTree n (Node x xs) = Node x $ map (takeTree n) xs

