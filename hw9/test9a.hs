{-
  Name: Noemi Turner
  File: test9a.hs
  Date: Spring 2022
  Desc: Basic tests for HW9. To execute from the command line using
        ghci, run: ghci test9a.hs -e main
-}

import HW9A

-- TODO: Add additional tests to for hw9a below
main = do
  -- my reverse tests
  putStrLn (assertTrue (null (myReverse [])) "myReverse 1")    
  putStrLn (assertEqual [1] (myReverse [1]) "myReverse 2")  
  putStrLn (assertEqual [2,1] (myReverse [1,2]) "myReverse 3")
  putStrLn (assertEqual ['a'..'z'] (myReverse ['z','y'..'a']) "myReverse 4")

  -- my last tests
  putStrLn (assertEqual 1 (myLast [1]) "myLast 1")
  putStrLn (assertEqual 2 (myLast [1,2]) "myLast 2")
  putStrLn (assertEqual 7 (myLast [1,2,3,4,5,6,7]) "myLast 3")

  -- my Init tests 
  putStrLn (assertEqual [] (myInit [1]) "myInit 1")
  putStrLn (assertEqual [1,2] (myInit [1,2,3]) "myInit 2")
  putStrLn (assertEqual ['a'..'y'] (myInit ['a'..'z']) "myInit 3")

  -- myMemb tests 
  putStrLn (assertEqual False (myMemb 3 []) "myMemb 1")
  putStrLn (assertEqual True (myMemb 3 [1,2,3,4]) "myMemb 2")
  putStrLn (assertEqual False (myMemb 3 [4,5,6,7,8,9]) "myMemb 3")

  -- myReplace tests
  putStrLn (assertEqual [7] (myReplace (1,7) [1]) "myReplace 1")
  putStrLn (assertEqual [] (myReplace (1,7) []) "myReplace 2")
  putStrLn (assertEqual [7,2,7,3] (myReplace (1,7) [1,2,1,3]) "myReplace 3")
  putStrLn (assertEqual [5,5,5,5,5] (myReplace (1,7) [5,5,5,5,5]) "myReplace 4")
  putStrLn (assertEqual [2,2,2,2,2,7] (myReplace (1,7) [2,2,2,2,2,1]) "myReplace 5")

  -- myReplaceAll tests 
  putStrLn (assertEqual ['b', 'b', 'd', 'd'] (myReplaceAll [('a','b'), ('c','d')] ['a', 'b', 'c', 'd'] ) "myReplaceAll 1")
  putStrLn (assertEqual  [3, 3, 3, 4] (myReplaceAll [(1,2), (2,3)] [1, 2, 3, 4]  ) "myReplaceAll 2")
  putStrLn (assertEqual  [3, 3, 3, 4] (myReplaceAll [(1,2), (2,3)] [3, 3, 3, 4]  ) "myReplaceAll 3")

  -- myElemSum tests
  putStrLn (assertEqual 10 (myElemSum 10 [15, 10, 25]) "myElemSum 1")
  putStrLn (assertEqual 12 (myElemSum 3 [3, 2, 3, 2, 3, 4, 3] ) "myElemSum 2")
  putStrLn (assertEqual 0 (myElemSum 3 []) "myElemSum 3")

  -- myRemDups tests 
  putStrLn (assertEqual ['c', 'b', 'a'] (myRemDups ['a', 'b', 'a', 'c', 'b', 'a']) "myRemDups 1") 
  putStrLn (assertEqual [10, 13, 11, 12] (myRemDups [10, 11, 13, 11, 12]) "myRemDups 2") 
  putStrLn (assertEqual [] (myRemDups (drop 1 [1])) "myRemDups 3")
  putStrLn (assertEqual [1,2,3,4,5] (myRemDups [1,2,3,4,5]) "myRemDups 4") 

  -- myListMax tests 
  putStrLn (assertEqual 12 (myListMax [7, 1, 9, 12, 10]) "myListMax 1")
  putStrLn (assertEqual 100 (myListMax [100,100,100,100]) "myListMax 2")
  putStrLn (assertEqual 1 (myListMax [1]) "myListMax 3")


  -- myMergeSort tests   
  putStrLn (assertEqual [1,2,3,4,5] (myMergeSort [5,4,3,2,1]) "myMergeSort 1")
  putStrLn (assertEqual [1,1,2,2] (myMergeSort [1,2,1,2]) "myMergeSort 2")
  putStrLn (assertEqual [1,2,3,4] (myMergeSort [1,2,3,4]) "myMergeSort 3")
  putStrLn (assertEqual ['a'..'z'] (myMergeSort ['z','y'..'a']) "myMergeSort 4")
  putStrLn (assertEqual [1,1,1,1,1,1,1,1,1,1,1,1,4,5] (myMergeSort [1,1,1,1,5,1,1,1,1,4,1,1,1,1]) "myMergeSort 5")
  putStrLn (assertEqual ['a'] (myMergeSort ['a']) "myMergeSort 6")
  putStrLn (assertEqual [] (myMergeSort $ drop 1 [2]) "myMergeSort 7")
  


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

