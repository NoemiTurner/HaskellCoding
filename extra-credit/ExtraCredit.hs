{-
   Name: Noemi Turner
   File: ExtraCredit.hs
   Date: Spring 2022
   Desc: Extra credit assignment to implement a simple key-value pair
         map ADT.
-}


module ExtraCredit (
  Map (MapElem, EmptyMap),
  insert,
  erase,
  contains,
  size,
  keys,
  keyVals,
  groupByKey,
  keyRange,
  keyJoin,
  reduceKeyVals
) where

data Map k v = MapElem k v (Map k v)
             | EmptyMap
           deriving (Show, Eq)


-- TODO: Implement the functions below. Be sure to carefully read the
-- instructions. For each function, the function type is provided.

-- (1) insert: adds kv-pair to front of map
-- The function insert takes a key, a value, and a map. The result is the input map with a new
-- element holding the key and value. The new element is placed at the front of the map. Note
-- that duplicate keys can exist in a given map.

insert :: k -> v -> Map k v -> Map k v
insert x y EmptyMap = MapElem x y EmptyMap
insert x y m = MapElem x y (m)

-- (2) erase: erases all kv-pairs with key
-- The function erase removes all of the given key pairs from the given map. If the key is not in
-- the map, then the original map is returned. Otherwise, the original map is returned without
-- the corresponding key-value pairs.

erase :: Eq k => k -> Map k v -> Map k v
erase key EmptyMap = EmptyMap
erase key (MapElem k v restOfMap) 
  | key == k = erase key restOfMap
  | otherwise = MapElem k v (erase key restOfMap)


-- (3) contains: true if map contains key
-- The function contains takes a key and a map and returns true if there is a key-value pair
-- element in the map that has the key, and false otherwise.

contains :: Eq k => k -> Map k v -> Bool
contains k EmptyMap = False
contains key (MapElem k v restOfMap)
  | k == key = True
  | otherwise = contains key restOfMap


-- (4) size: returns number of kv-pairs
-- The function size returns the number of key-value pairs in the map. The size of the empty
-- map is 0.

size :: Map k v -> Int
size EmptyMap = 0
size (MapElem k v restOfMap) = 1 + size (restOfMap)


-- (5) keys: return list of unique keys
-- The function keys returns the list of unique keys in the map (i.e., the return list should not
-- return duplicate keys). If the map is empty, keys should return the empty list.

keys :: Eq k => Map k v -> [k]
keys EmptyMap = []
keys (MapElem k v restOfMap) = k : keys (erase k restOfMap)


-- (6) keyVals: return list of values for key
-- The function keyVals takes a key and returns a list of all the values of that key in the map.
-- If the key is not in the map, the function should return the empty list.

keyVals :: Eq k => k -> Map k v -> [v]
keyVals key theMap | contains key theMap == False = [] -- the key is not in the map
keyVals key (MapElem k v restOfMap) -- otherwise, the key is in the map, so continue looking for the keyVals
  | key == k = v : keyVals key restOfMap
  | otherwise = keyVals key restOfMap


-- (7) groupByKey: create map from k -> [v]
-- The function groupByKey takes a map and returns a new map that contains one element
-- per key such that the element contains a list of all the values of that key in the 
-- original map. You can reuse your keyVals function to implement groupByKey. 
-- As an example, groupByKey (MapElem 'a' 10 (MapElem 'a' 20 (MapElem 'b' 15 EmptyMap))) 
-- should return the map (MapElem 'a' [10,20] (MapElem 'b' [15])).

groupByKey :: Eq k => Map k v -> Map k [v]
groupByKey EmptyMap = EmptyMap
groupByKey (MapElem k v restOfMap) = let m = MapElem k (keyVals k (MapElem k v restOfMap))
                                         mapWithoutFirstKey = erase k restOfMap
                                      in
                                      m (groupByKey mapWithoutFirstKey)


-- (8) keyRange: return kv-pairs with key k s.t. k1 <= k <= k2
-- The function keyRange takes two keys k1 and k2, and a map, and returns the original map
-- whose elements have the key k such that k1 ≤ k ≤ k2. If no such keys exist in the input map,
-- then keyRange should return the empty map.

keyRange :: Ord k => k -> k -> Map k v -> Map k v
keyRange k1 k2 EmptyMap = EmptyMap
keyRange k1 k2 (MapElem key v restOfMap) 
  | k1 <= key && key <= k2 = MapElem key v (keyRange k1 k2 restOfMap)
  | otherwise = keyRange k1 k2 restOfMap


-- (9) keyJoin: combine all values of identical keys in given maps
-- The function keyJoin takes two maps m1 and m2 and combines them into a new map as
-- follows. For each key k with key values [v1, . . . , vi] in m1 and [vj , . . . , vn] in m2, the output
-- map should have a corresponding element with key k and list of values [v1, . . . , vi, vj , . . . , vn].
-- Only keys that are in both maps should be in the output map.

keyJoin :: Eq k => Map k v -> Map k v -> Map k [v]
keyJoin EmptyMap _ = EmptyMap
keyJoin _ EmptyMap = EmptyMap
keyJoin map1 map2 = let m1 = groupByKey map1
                        m2 = groupByKey map2
                      in joinMaps m1 m2
                    where
                    joinMaps :: Eq k => Map k [v] -> Map k [v] -> Map k [v]
                    joinMaps EmptyMap _ = EmptyMap
                    joinMaps _ EmptyMap = EmptyMap
                    joinMaps (MapElem k1 v1 rest1) (MapElem k2 v2 rest2)
                      | k1 == k2 = MapElem k1 (v1 ++ v2) (joinMaps rest1 rest2)
                      | otherwise = joinMaps rest1 rest2


-- (10) reduceKeyVals: higher-order function to apply function to
--                     list of values for each key
-- The function reduceKeyVals is a higher order function that employs an input function to
-- eliminate duplicate keys in the given map by “reducing” the duplicates to a single key-value
-- pair. The function supplied to reduceKeyVals converts a list of values (i.e., of the same key)
-- into a single value.

reduceKeyVals :: Eq k => ([v] -> v) -> Map k v -> Map k v
reduceKeyVals _ EmptyMap = EmptyMap
reduceKeyVals f map = let m = groupByKey map
                        in reduce f m
                      where 
                      reduce :: Eq k => ([v] -> v) -> Map k [v] -> Map k v
                      reduce _ EmptyMap = EmptyMap
                      reduce f (MapElem k vals rest) = MapElem k (f vals) (reduce f rest)