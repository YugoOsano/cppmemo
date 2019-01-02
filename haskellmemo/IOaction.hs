-- from Haskell Nyumon authored by Honma et al.
{-
  operator >>= takes
   IO a       as its first argument
   a -> IO b  as its second argument,
   where a has to be a same type

  the simplest example in ghci is:
   getLine >>= putStrLn
-}

-- this is equivalent to what's done in main
{-
main :: IO ()
main = do
  x <- getLine
  y <- getLine
  putStrLn $ "The first input is: " ++ x
  putStrLn $ "The second input is: " ++ y
-}

main :: IO ()
main =
  getLine >>= \x ->
  getLine >>= \y ->
  putStrLn ("The first input is: " ++ x) >>
  putStrLn ("The second input is: " ++ y)

 {-
  >> connects an action without a return value
     with another action
 -}
  