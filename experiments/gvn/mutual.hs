module Mutual where

even' :: Integer -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Integer -> Bool
odd' 0 = False
odd' n = even' (n - 1)
