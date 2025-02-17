
module Problems where

import Data.List

-- 01 The last elemento of a list
myLast :: [a] -> a
myLast []            = error "empty list"
myLast [a]           = a
myLast (head : tail) = myLast tail

-- 02 Second to last tail in a list
secondLast :: [a] -> a
secondLast []        = error "emtpy list"
secondLast [a]       = error "single item list"
secondLast [a, b]    = a
secondLast (head : tail) = secondLast tail

-- 03 K'th element in list
atK :: [a] -> Int -> a
atK _ 0 = error "K must be 1 or higher"
atK list index = list!!(index - 1)

-- 04 myLength
myLength :: [a] -> Int
myLength []            = 0
myLength (head : tail) = 1 + myLength tail

-- 05 Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (head : tail) = myReverse tail ++ [head]

-- 06 Is list a palindrome?
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == myReverse list

-- 07 Flatten nested lists
flatten :: [[a]] -> [a]
flatten []            = []
flatten (head : tail) = [head!!x | x <- [0..y]] ++ flatten tail
                        where y = length head - 1

-- 08 Eliminate consecutives
compress :: Eq a => [a] -> [a]
compress list = map head . group $ list

-- 09 Pack
pack :: Eq a => [a] -> [[a]]
pack list = group list

-- 10 Run length encode a list
rle :: Eq a => [a] -> [(Int, a)]
rle list = map (\x -> (length x, head x)) . group $ list


-- 11 rle using multiple and single
data Item a = Multiple Int a | Single a
 deriving(Show)

rle2 :: Eq a => [a] -> [Item a]
rle2 list = map listItem (group list)
 where 
  listItem [item] = Single item
  listItem list   = Multiple (length list) (head list) 

-- 12 decode using multiple and single
decode2 :: Eq a => [Item a] -> [a]
decode2 []   = []
decode2 list = concat $ map helper list
 where
  helper (Multiple count item) = [item | _ <- [1..count]] 
  helper (Single item)         = [item]

-- 13 rle without group
rle3 :: Eq a => [a] -> [Item a]
rle3 [item]  = [Single item]
rle3 (theHead : theTail)   
 | theHead == nextHead = Multiple (1 + count) theHead : rle3 nextItem
 | otherwise           = Single theHead : rle3 theTail
   where
    nextHead = head theTail
    count    = length matching
    matching = takeWhile (== theHead) theTail
    nextItem = dropWhile (== theHead) theTail

 -- 14 duplicate list items
dupli :: [a] -> [a]
dupli list = list >>= (\x -> [x, x])

-- 15 replicate list items n times
repli :: [a] -> Int -> [a]
repli list n = list >>= (\x ->  [x | _ <- [0.. (n-1)]])

-- 16 drop every nth item
dropEvery :: [a] -> Int -> [a]
dropEvery list n = [list!!(x - 1) | x <- [1..(length list)], x `mod` n /= 0]

-- 17 split list at n
split :: [a] -> Int -> ([a], [a])
split list n = (take n list, drop n list)

-- 18 slice
slice :: [a] -> Int -> Int -> [a]
slice list start fin = [list!!x | x <- [start..fin]]

-- 19 rotate left
rotate :: [a] -> Int -> [a]
rotate list 0          = list
rotate (head : tail) n = rotate (tail ++ [head]) (n-1)

-- 20 remove n'th element from list
removeAt :: Int -> [a] -> [a]
removeAt n []   = []
removeAt 1 (head : tail) = tail
removeAt n (head : tail) = head : removeAt (n-1) tail  

-- 21 insert element at X in a list
insertAt :: a -> [a] -> Int -> [a]
insertAt thing list n = take (n - 1) list ++ [thing] ++ drop (n - 1) list

-- 22 make a range of integers
range :: Int -> Int -> [Int]
range x y
 | x == y       = [x]
 | x >  y       = [y, y+1.. x]
 | otherwise    = [x, x+1.. y]

 -- 23 