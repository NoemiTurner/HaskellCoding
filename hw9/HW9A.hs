{-
   Name: Noemi Turner
   File: HW9A.hs
   Date: Spring 2022
   Desc: Haskell Functions
-}


module HW9A (
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


-- TODO: Implement the following functions WITHOUT USING pattern
-- matching, and instead using if-then-else. For each function,
-- provide the function type. An example is provided for (1), however,
-- the actual definition is still required.


-- (1). myReverse
myReverse :: [a] -> [a]
myReverse xs = if length xs <= 1
                then xs
               else myReverse (drop 1 xs) ++ (take 1 xs)


-- (2). myLast
myLast :: [a] -> a
myLast xs = if length xs < 1
              then error "Empty List"
            else ( if length xs == 1
                      then head xs
                    else myLast (drop 1 xs)
            )


-- (3). myInit
myInit :: [a] -> [a]
myInit xs = if null xs
              then error "Empty List"
            else (if length xs == 1
                    then []
                  else take (length xs - 1) xs
            )


-- (4). myMemb
myMemb :: Eq a => a -> [a] -> Bool
myMemb x xs = if length xs < 1
                then False
              else (if head xs == x
                      then True
                    else myMemb x $ tail xs
              )


-- (5). myReplace
myReplace :: Eq a => (a, a) -> [a] -> [a]
myReplace (a,b) xs = if null xs
                        then []
                      else (if head xs == a
                              then b : myReplace (a,b) (tail xs)
                            else head xs : myReplace (a,b) (tail xs)
                      )


-- (6). myReplaceAll
myReplaceAll :: Eq a => [(a, a)] -> [a] -> [a]
myReplaceAll [(a,b),(c,d)] xs =  let t = [(a,b),(c,d)] in myReplace (head (drop 1 t)) $ myReplace (head t) xs


-- (7). myElemSum
myElemSum :: (Num a, Eq a) => a -> [a] -> a
myElemSum x xs = if null xs
                    then 0
                  else (if head xs == x
                            then x + myElemSum x (drop 1 xs)
                        else myElemSum x (drop 1 xs)
                  )

-- (8). myRemDups
myRemDups :: Eq a => [a] -> [a]
myRemDups xs = if null xs
                   then []
                else (let y = head xs
                          ys = tail xs
                        in if myMemb y ys
                                then myRemDups ys
                              else y : myRemDups ys
                )


-- (9). myListMax
myListMax :: Ord a => [a] -> a
myListMax xs = if null xs
                  then error "Empty List"
                else (if length xs == 1
                        then head xs
                      else (let y = head xs
                                ys = tail xs
                            in if y > myListMax ys
                                then y
                            else myListMax ys
                      )
                )

-- (10). myMergeSort
myMergeSort :: Ord a => [a] -> [a]
myMergeSort xs = if length xs <= 1
                    then xs
                  else (
                    let left = take (length xs `div` 2) xs
                        right = drop (length xs `div` 2) xs
                    in
                      merge (myMergeSort left, myMergeSort right)
                  )
                 where
                 merge (left, right) = if null left && null right
                                          then []
                                        else(if null left
                                                then right
                                              else (if null right
                                                      then left
                                                    else (if (head left) <= (head right)
                                                            then (head left) : merge (tail left, right)
                                                          else (head right) : merge (left, tail right)
                                                    )
                                              )
                                        )
