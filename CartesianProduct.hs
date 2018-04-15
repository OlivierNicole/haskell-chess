module CartesianProduct ( (>*<)
                        , sq
                        ) where

(>*<) :: [a] -> [b] -> [(a, b)]
l >*< l' = concatMap (\x -> map ((,) x) l') l

sq :: [a] -> [(a, a)]
sq l = l >*< l

