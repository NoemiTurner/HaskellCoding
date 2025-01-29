{-
   Name: Noemi Turner
   File: HW9B.hs
   Date: Spring 2022
   Desc: Haskell Functions
-}


module HW9B (
  myReverse,
  myLast,
  myInit,
  myMemb,
  myReplace,
  myReplaceAll,
  myElemSum,
  myRemDups,
  myListMax,
  myMergeSort
) where


-- TODO: Implement the following functions USING PATTERN MATCHING. You
-- CANNOT use any if-then-else expressions in your
-- implementations. Again, provide the function types for each
-- function (see HW9A.hs for an example). 

-- (1). myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- (2). myLast 
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast xs | length xs == 1 = head xs
myLast (x:xs) = myLast xs


-- (3). myInit
myInit :: [a] -> [a] 
myInit [] = error "Empty List"
myInit xs | length xs == 1 = []
myInit xs = take (length xs - 1) xs


-- (4). myMemb 
myMemb :: Eq a => a -> [a] -> Bool
myMemb x xs | length xs < 1 = False
myMemb x xs | head xs == x = True
myMemb x xs = myMemb x $ tail xs


-- (5). myReplace
myReplace :: Eq a => (a, a) -> [a] -> [a]
myReplace (a,b) [] = []
myReplace (a,b) xs | head xs == a = b : myReplace (a,b) (tail xs)
myReplace (a,b) xs = head xs : myReplace (a,b) (tail xs)


-- (6). myReplaceAll
myReplaceAll :: Eq a => [(a, a)] -> [a] -> [a]
myReplaceAll [(a,b),(c,d)] [] = []
myReplaceAll [(a,b),(c,d)] xs = myReplace (head (drop 1 [(a,b),(c,d)])) $ myReplace (head [(a,b),(c,d)]) xs

-- (7). myElemSum
myElemSum :: (Num a, Eq a) => a -> [a] -> a
myElemSum x [] = 0
myElemSum x xs | head xs == x = x + myElemSum x (drop 1 xs)
myElemSum x xs = myElemSum x (drop 1 xs)

-- (8). myRemDups
myRemDups :: Eq a => [a] -> [a]
myRemDups [] = []
myRemDups xs | myMemb (head xs) (tail xs) = myRemDups (tail xs)
myRemDups xs = head xs : myRemDups (tail xs)


-- (9). myListMax
myListMax :: Ord a => [a] -> a
myListMax [] = error "Empty List"
myListMax xs | length xs == 1 = head xs
myListMax xs | (head xs) > myListMax (tail xs) = (head xs) 
myListMax xs = myListMax (tail xs)


-- (10). myMergeSort
myMergeSort :: Ord a => [a] -> [a]
myMergeSort [] = []
myMergeSort xs | length xs == 1 = xs
myMergeSort xs = let (left, right) = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
                    in merge (myMergeSort left) (myMergeSort right)
                  where
                  merge :: Ord a => [a] -> [a] -> [a]
                  merge [] ys = ys
                  merge xs [] = xs
                  merge (x:xs) (y:ys)
                    | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys