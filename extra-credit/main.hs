{-
  Name: Noemi Turner
  File: main.hs
  Date: Spring 2022
  Desc: Basic tests for the optional Extra Credit assignment.  To
        execute from the command line using ghci, run: ghci main.hs -e
        main
-}

import ExtraCredit

-- TODO: Add additional example maps as needed here
m1 = MapElem 'a' 1 (MapElem 'c' 3 (MapElem 'd' 4 (MapElem 'b' 2 EmptyMap)))
m2 = MapElem "a" 10 EmptyMap
m3 = MapElem "z" 1 (MapElem "a" 10 EmptyMap)
m4 = MapElem "z" 8 (MapElem "z" 1 (MapElem "a" 10 EmptyMap))
m5 = MapElem 'e' 7 (MapElem 'a' 1 (MapElem 'c' 3 (MapElem 'd' 4 (MapElem 'b' 2 EmptyMap))))
m6 = MapElem 'a' [1] (MapElem 'c' [3] (MapElem 'd' [4] (MapElem 'b' [2] EmptyMap)))
m7 = MapElem "z" [8,1] (MapElem "a" [10] EmptyMap)
m8 = MapElem 'a' 10 (MapElem 'b' 20 (MapElem 'c' 30 EmptyMap))
m9 = MapElem 'a' 30 (MapElem 'a' 40 (MapElem 'b' 15 (MapElem 'd' 10 EmptyMap)))
m10 = MapElem 'a' [10,30,40] (MapElem 'b' [20,15] EmptyMap)
m11 = MapElem "a" [10,10] EmptyMap
m12 = MapElem "z" 9 (MapElem "a" 10 EmptyMap)
m13 = MapElem 'a' 70 (MapElem 'b' 15 (MapElem 'd' 10 EmptyMap))


-- TODO: Add additional tests of your functions below
main = do
  -- contains tests
  putStrLn (assertTrue (contains 'a' m1) "contains 1") 
  putStrLn (assertTrue (contains 'b' m1) "contains 2") 
  putStrLn (assertTrue (contains "a" m2) "contains 3") 
  putStrLn (assertFalse (contains "z" m2) "contains 4") 

  -- insert tests
  putStrLn (assertEqual m3 (insert "z" 1 m2) "insert 1") 
  putStrLn (assertEqual m4 (insert "z" 8 m3) "insert 2") 
  putStrLn (assertEqual m5 (insert 'e' 7 m1) "insert 3")
  putStrLn (assertEqual m2 (insert "a" 10 EmptyMap) "insert 4")

  -- erase tests
  putStrLn (assertEqual m1 (erase 'e' m5) "erase 1")
  putStrLn (assertEqual m2 (erase "z" m4) "erase 2") 
  putStrLn (assertEqual m2 (erase "n" m2) "erase 3")
  putStrLn (assertEqual EmptyMap (erase "a" m2) "erase 4")

  -- size tests
  putStrLn (assertEqual 4 (size m1) "size 1")
  putStrLn (assertEqual 1 (size m2) "size 2")
  putStrLn (assertEqual 3 (size m4) "size 3")

  -- keys tests
  putStrLn (assertEqual ['a','c','d','b'] (keys m1) "keys 1")
  putStrLn (assertEqual ["a"] (keys m2) "keys 2")
  putStrLn (assertEqual [] (keys EmptyMap :: [Char]) "keys 3")
  putStrLn (assertEqual ["z", "a"] (keys m4) "keys 4")

  -- keyVals tests
  putStrLn (assertEqual [] (keyVals 'n' m1) "keyVals 1")
  putStrLn (assertEqual [3] (keyVals 'c' m1) "keyVals 2")
  putStrLn (assertEqual [8,1] (keyVals "z" m4) "keyVals 3")

  -- groupByKey tests
  putStrLn (assertEqual m6 (groupByKey m1) "groupByKey 1")
  putStrLn (assertEqual m7 (groupByKey m4) "groupByKey 2")

  -- keyRange tests
  putStrLn (assertEqual EmptyMap (keyRange 'q' 'z' m1) "keyRange 1")
  putStrLn (assertEqual m1 (keyRange 'a' 'd' m1) "keyRange 2")
  putStrLn (assertEqual m1 (keyRange 'a' 'd' m5) "keyRange 3")
  putStrLn (assertEqual m2 (keyRange "a" "b" m3) "keyRange 4")
  putStrLn (assertEqual m2 (keyRange "a" "a" m3) "keyRange 5")

  -- keyJoin tests
  putStrLn (assertEqual m10 (keyJoin m8 m9) "keyJoin 1")
  putStrLn (assertEqual m11 (keyJoin m2 m2) "keyJoin 2")
  putStrLn (assertEqual EmptyMap (keyJoin m1 EmptyMap) "keyJoin 3")
  putStrLn (assertEqual EmptyMap (keyJoin EmptyMap m1) "keyJoin 4")
  putStrLn (assertEqual EmptyMap (keyJoin (erase "a" m2) (erase "a" m2)) "keyJoin 5") -- two empty maps are passed in


  -- reduceKeyVals tests
  putStrLn (assertEqual m12 (reduceKeyVals sum m4) "reduceKeyVals 1")
  putStrLn (assertEqual m13 (reduceKeyVals sum m9) "reduceKeyVals 2")

  
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

