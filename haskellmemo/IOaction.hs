-- from Haskell Nyumon authored by Honma et al.
{-
  operator >>= takes
   IO a       as its first argument
   a -> IO b  as its second argument,
   where a has to be a same type

  the simplest example in ghci is:
   getLine >>= putStrLn
-}

repeatString :: String -> String
repeatString x = x ++ x

-- this is equivalent to what's done in main commented out
main :: IO ()
main = do
  x <- getLine -- <- operator can be used only in do range
               --  https://qiita.com/7shi/items/85afd7bbd5d6c4115ad6
  y <- getLine
  let y_str = repeatString y
  
  putStrLn $ "The first input is: " ++ x
  putStrLn $ "The second input is: " ++ y_str

{-
main :: IO ()
main =
  getLine >>= \x ->
  getLine >>= \y ->
  putStrLn ("The first input is: " ++ x) >>
  putStrLn ("The second input is: " ++ y)
-}
 {-
  >> connects an action without a return value
     with another action
 -}
  
