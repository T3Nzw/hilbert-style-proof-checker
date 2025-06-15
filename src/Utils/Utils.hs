module Utils where

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy cmp x (y : ys) = x `cmp` y || elemBy cmp x ys
