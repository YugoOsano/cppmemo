import Geometry

-- compile: ghc helloworld.hs 
main = do
  putStrLn "hello, world!"

  print $ width (Rectangle 3 4)
  print $ height (Rectangle 3 4)
  print $ area (Triangle 3 4)
  print $ area (Rectangle 3 4)

