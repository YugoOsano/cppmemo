{-
parser practice from:
https://qiita.com/7shi/items/b8c741e78a96ea2c10fe
-}

import qualified Data.Char as Char 

-- [t] -> (t, [t])
-- ('a',"bc") will be returned by "abc"
anyChar (x:xs) = (x, xs)

satisfy f (x:xs)
  | f x = (x, xs)

digit = satisfy Char.isDigit

main = putStrLn ""
