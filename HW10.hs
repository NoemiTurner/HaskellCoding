{-
   Name: Noemi Turner
   File: HW10.hs
   Date: Spring 2022
   Desc: Set ADT implementation
-}


module HW10 (
  Set (Elem, EmptySet),
  member,
  add,
  remove,
  size,
  subset,
  union,
  intersect,
  difference,
  filterSet,
  toList
) where

data Set a = Elem a (Set a)
           | EmptySet
           deriving (Show, Eq)

-- TODO: Implement each of the functions below. For each function
-- include the function's type. The type of the member function (as well
-- as a "stubbed out" implementation) is given below as an
-- example. You are NOT allowed to use any if-then-else expressions in
-- your implementation. See the homework assignment for additional
-- information and restrictions.

-- (1). member
-- The function member takes a value and a set and returns true if the value is an element of the
-- given set, and false otherwise. E.g., member 'b' (Elem 'b' (Elem 'a' EmptySet)) is true
-- whereas member 'b' (Elem 'a' EmptySet) is false.
member :: Eq a => a -> Set a -> Bool
member x EmptySet = False
member x (Elem y set) = x == y || member x set

-- (2). add
-- The function add takes a value and a set and returns the original set with the value added
-- as the last element. As a simple example, add 'a' (Elem 'b' EmptySet) should return the
-- set Elem 'b' (Elem 'a' EmptySet). Note that if the value is already an element of the set,
-- the original set should be returned.
add :: Eq a => a -> Set a -> Set a
add x EmptySet = (Elem x EmptySet)
add x (Elem y set) 
  | x == y = (Elem y set) 
  | otherwise = (Elem y (Elem x set))


-- (3). remove
-- The function remove takes a value and a set and returns the original set without the given
-- value. If the value isn’t in the set, then the original set is returned
remove :: Eq a => a -> Set a -> Set a
remove x EmptySet = EmptySet
remove x (Elem y set) 
  | x == y = set
  | otherwise = Elem y (remove x set)


-- (4). size
-- The function size returns the number of elements in the set. Note that the size of the empty
-- set is 0.
size :: Set a -> Int 
size EmptySet = 0
size (Elem y set) = 1 + size set


-- (5). subset
-- The function subset returns true if the first set is a subset of the second set, and false
-- otherwise. Note that the empty set is a subset of every set.
subset :: Eq a => Set a -> Set a -> Bool
subset EmptySet x = True
subset x EmptySet = False
subset (Elem x setX) (Elem y setY) 
  | (x == y) && (setX == EmptySet) = True
  | x == y = subset setX setY
  | otherwise = subset (Elem x setX) setY


-- (6). union
-- The function union takes two sets and returns a set with all elements of the first and second
-- set. Note that the result must still be a valid set (i.e., no duplicates).
union :: Eq a => Set a -> Set a -> Set a
union x EmptySet = x
union EmptySet x = x
union (Elem x setX) setY
  | member x setY = union setX setY
  | otherwise = Elem x (union setX setY)


-- (7). intersect
-- The function intersect takes two sets and returns a set with the elements that are in both
-- sets. Note that the result must still be a valid set (i.e., no duplicates).
intersect :: Eq a => Set a -> Set a -> Set a
intersect x EmptySet = EmptySet
intersect EmptySet x = EmptySet
intersect (Elem x setX) setY
  | member x setY = Elem x (intersect setX setY)
  | otherwise = intersect setX setY



-- (8). difference
-- The function difference takes two sets and returns a set with all elements in the first set
-- that are not in the second set. The result must be a valid set.
difference :: Eq a => Set a -> Set a -> Set a
difference EmptySet setX = EmptySet
difference setX EmptySet = setX
difference (Elem x setX) setY
  | member x setY = difference setX setY
  | otherwise = Elem x (difference setX setY)



-- (9). filterSet
-- The function filterSet takes a predicate (unary Boolean function) and a set, and returns
-- a new set with all elements of the original set (in the same order) that satisfy the predicate.
-- Note that this function should behave in a similar way as the Haskell filter function over
-- lists.
--   Note: functions can be passed in as named arguments and called
--   within the body of the function. For example, here is a simple
--   implementation of the standard map function over lists:
--       map _ [] = []
--       map f (x:xs) = f x : map f xs

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet _ EmptySet = EmptySet
filterSet predicate (Elem x setX) 
  | predicate x = Elem x (filterSet predicate setX)
  | otherwise = filterSet predicate setX


-- (10). toList
-- The function toList takes a set and returns a list with the values in the set (in the same
-- order as the original set). Note that you cannot use the toList function in any of your other
-- functions above.
toList :: Set a -> [a]
toList EmptySet = []
toList (Elem x setX) = x : toList setX