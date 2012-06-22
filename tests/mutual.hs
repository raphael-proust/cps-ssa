module Mutual where

f :: Integer -> Bool
f 0 = True
f n = g (n - 1)

g :: Integer -> Bool
g 0 = False
g n = f (n - 1)
