module MutualDead where

f :: Integer -> Bool
f 0 = True
f n = if foo
        then g 42
        else g 34
      where
      foo = g n

g :: Integer -> Bool
g 0 = False
g n = f n
