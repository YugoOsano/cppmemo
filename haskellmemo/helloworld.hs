import Geometry

-- compile: ghc helloworld.hs 
main = do
  putStrLn "hello, world!"

  print $ width (Rect 3 4)
  print $ height (Rect 3 4)
  print $ area (Tri 3 4)
  print $ area (Rect 3 4)

