module MutualInf where

f :: Integer -> Bool
f 0 = True
f n = g n

g :: Integer -> Bool
g 0 = False
g n = f n
