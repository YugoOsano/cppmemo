main = do
  -- list comprehension
  -- https://sites.google.com/site/kiyohiko90091e/programming/haskell/gen-lists
  -- [map-func x | x <- xs, filter-predicate]
  print [x | x <- [0 .. 100]]
