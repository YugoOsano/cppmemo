main = do
  -- list comprehension
  -- https://sites.google.com/site/kiyohiko90091e/programming/haskell/gen-lists
  -- [map-func x | x <- xs, filter-predicate]
  print [x | x <- [0 .. 100]]

  -- about map function
  -- https://qiita.com/knknkn1162/items/92de8dd250ff94cd86f1
  -- the following two are same
  print  (map (+2) [0 .. 10]) 
  print $ map (+2) [0 .. 10]

  -- map with lambda
  -- :type of map:
  -- (a -> b) -> [a] -> [b]
  print $ map (\x -> x+2) [0 .. 10]

  -- create a Maybe monad
  -- https://qiita.com/7shi/items/c7d7eec066af0fe0688d
  let a = Just 1
      b = return 1 :: Maybe Int
  print (a, b)
