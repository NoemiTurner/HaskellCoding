{-
  Name: Noemi Turner
  File: main.hs
  Date: Spring 2022
  Desc: Basic tests for HW10. To execute from the command line using
        ghci, run: ghci main.hs -e main
-}

import HW10

-- Additional set examples as needed here
s1 = Elem 'a' (Elem 'c' (Elem 'd' (Elem 'b' (Elem 'e' EmptySet))))
s2 = (Elem 'b' EmptySet)
s3 = (Elem 'b' (Elem 'a' EmptySet))
s4 = (Elem 'x' (Elem 'y' (Elem 'z' EmptySet)))
s5 = (Elem 'b' (Elem 'a' (Elem 'x' (Elem 'y' (Elem 'z' EmptySet)))))
s6 = (Elem 'c' (Elem 'd' (Elem 'e' EmptySet)))
s7 = (Elem 'a' EmptySet)
s8 = Elem 'a' (Elem 'c' (Elem 'd' (Elem 'e' (Elem 'b' EmptySet))))
s9 = (Elem 'a' (Elem 'b' EmptySet))

isAVowel x = x == 'a' 

-- Additional tests to for hw10 below
main = do
  -- member tests
  putStrLn (assertTrue (member 'a' s1) "member 1")
  putStrLn (assertTrue (member 'b' s1) "member 2")
  putStrLn (assertTrue (member 'c' s1) "member 3")
  putStrLn (assertTrue (member 'd' s1) "member 4")
  putStrLn (assertTrue (member 'e' s1) "member 5")
  putStrLn (assertFalse (member 'f' s1) "member 6")

  -- add tests
  putStrLn (assertEqual s3 (add 'a' s2) "add 1")
  putStrLn (assertEqual s2 (add 'b' s2) "add 2")
  putStrLn (assertEqual s2 (add 'b' EmptySet) "add 3")

  -- remove tests
  putStrLn (assertEqual (Elem 'a' EmptySet) (remove 'b' s3) "remove 1")
  putStrLn (assertEqual (Elem 'b' EmptySet) (remove 'a' s3) "remove 2")
  putStrLn (assertEqual s3 (remove 'z' s3) "remove 3")

  -- size tests
  putStrLn (assertEqual 0 (size EmptySet) "size 1")
  putStrLn (assertEqual 1 (size s2) "size 2")
  putStrLn (assertEqual 5 (size s1) "size 3")


  -- subset tests
  putStrLn (assertTrue (subset EmptySet s1) "subset 1")
  putStrLn (assertFalse (subset s1 s4) "subset 2")
  putStrLn (assertTrue (subset s2 s1) "subset 3")
  putStrLn (assertFalse (subset s1 s3) "subset 4")
  putStrLn (assertFalse (subset s1 EmptySet) "subset 5")


  -- union tests
  -- The function union takes two sets and returns a set with all elements of the first and second
  -- set. Note that the result must still be a valid set (i.e., no duplicates).
  putStrLn (assertEqual s8 (union s1 s2) "union 1")
  putStrLn (assertEqual s1 (union s1 EmptySet) "union 2")
  putStrLn (assertEqual s1 (union EmptySet s1) "union 3")
  putStrLn (assertEqual s5 (union s3 s4) "union 4")


  -- intersect tests
  -- The function intersect takes two sets and returns a set with the elements that are in both
  -- sets. Note that the result must still be a valid set (i.e., no duplicates).
  putStrLn (assertEqual s2 (intersect s1 s2) "intersect 1")
  putStrLn (assertEqual s1 (intersect s1 s1) "intersect 2")
  putStrLn (assertEqual EmptySet (intersect s1 s4) "intersect 3")
  putStrLn (assertEqual s2 (intersect s2 s3) "intersect 3")


  -- difference tests
  -- The function difference takes two sets and returns a set with all elements in the first set
  -- that are not in the second set. The result must be a valid set.
  putStrLn (assertEqual s7 (difference s3 s2) "difference 1")
  putStrLn (assertEqual s6 (difference s1 s3) "difference 2")
  putStrLn (assertEqual s9 (difference s1 s6) "difference 3")
  putStrLn (assertEqual EmptySet (difference s1 s1) "difference 4")
  putStrLn (assertEqual EmptySet (difference EmptySet s1) "difference 5")
  putStrLn (assertEqual s1 (difference s1 EmptySet) "difference 6")


  -- filterSet tests
  -- The function filterSet takes a predicate (unary Boolean function) and a set, and returns
  -- a new set with all elements of the original set (in the same order) that satisfy the predicate.
  -- Note that this function should behave in a similar way as the Haskell filter function over
  -- lists.
  putStrLn (assertEqual s7 (filterSet isAVowel s3) "filterSet 1")
  putStrLn (assertEqual EmptySet (filterSet isAVowel s4) "filterSet 2")
  putStrLn (assertEqual EmptySet (filterSet isAVowel EmptySet) "filterSet 3")


  -- toList tests
  putStrLn (assertEqual ['b'] (toList s2) "toList 1")
  putStrLn (assertTrue (null (toList EmptySet)) "toList 2")
  putStrLn (assertEqual ['x','y','z'] (toList s4) "toList 3")



assertEqual :: (Show a, Eq a) => a -> a -> String -> String
assertEqual x y s =
  if x == y then
    s ++ " [Pass]"
  else
    s ++ " [Fail] ... expecting " ++ (show x) ++ " found " ++ (show y)


assertTrue :: Bool -> String -> String
assertTrue x s = assertEqual True x s


assertFalse :: Bool -> String -> String
assertFalse x s = assertEqual False x s

