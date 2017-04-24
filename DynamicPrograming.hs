module DynamicProgramming where

import Data.Array
  (Array
  ,(!)
  ,listArray
  ,range)

-- | Slow, can't use prop
--
-- Examples:
--
-- >>> editDist "sitting" "kitten"
-- 3
--
-- >>> editDist "brother" "bother"
-- 1

editDist :: String -> String -> Int
editDist [] bz = length bz
editDist az [] = length az
editDist az@(a:as) bz@(b:bs) =
  if a == b then
    editDist as bs
  else
    minimum [insert, delete, modify]
  where
    insert = editDist az bs + 1
    delete = editDist as bz + 1
    modify = editDist as bs + 1

-- | Fast, can use prop :D
--
-- Examples:
--
-- >>> editDistDP "sitting" "kitten"
-- 3
--
-- >>> editDistDP "brother" "bother"
-- 1
--
-- prop> \s1 s2 -> editDistDP s1 s2 == editDistDP s2 s1

editDistDP :: String -> String -> Int
editDistDP s1 s2 =
  dist m n
  where
    (m, n) = (length s1, length s2)
    a = listArray (1, m) s1
    b = listArray (1, n) s2
    index = ((0,0), (m,n))
    dists = listArray index [dist i j | (i, j) <- range index]
    dist :: Int -> Int -> Int
    dist i 0 = i
    dist 0 j = j
    dist i j
      | a ! i == b ! j = dists ! (i - 1, j - 1)
      | otherwise = minimum [insert, delete, modify]
      where
        insert = dists ! (i - 1, j) + 1
        delete = dists ! (i, j - 1) + 1
        modify = dists ! (i - 1, j - 1) + 1

-- TODO record actions

data Action
  = Match
  | Insert
  | Delete
  | Modify

cost :: Action -> Int
cost Match = 0
cost _ = 1

editActs :: String -> String -> [Action]
editActs = undefined
