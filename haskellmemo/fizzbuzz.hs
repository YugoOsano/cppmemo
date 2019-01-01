fizzbuzz :: Integer -> [String]
fizzbuzz 0 = []
fizzbuzz n
    | n `mod` 15 == 0 = fizzbuzz(n - 1) ++ ["fizzbuzz"]
    | n `mod` 3 == 0 = fizzbuzz(n - 1) ++ ["fizz"]
    | n `mod` 5 == 0 = fizzbuzz(n - 1) ++ ["buzz"]
fizzbuzz n = fizzbuzz(n - 1) ++ [show n]

{- the above vertical bars are refered to as guard
   A Bool formula is followed after every guard
-}

-- a typical recursive function
factorial :: Int -> Int
factorial n
    | n == 0 = 1
factorial n = n * factorial (n-1)

main :: IO ()
main = do
  putStrLn "hello"
  print (fizzbuzz 100)
  -- $ operator is right-associative
  print $ fizzbuzz 100
  print $ factorial 5  
  
