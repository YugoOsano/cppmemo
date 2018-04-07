fizzbuzz :: Integer -> [String]
fizzbuzz 0 = []
fizzbuzz n
    | n `mod` 15 == 0 = fizzbuzz(n - 1) ++ ["fizzbuzz"]
    | n `mod` 3 == 0 = fizzbuzz(n - 1) ++ ["fizz"]
    | n `mod` 5 == 0 = fizzbuzz(n - 1) ++ ["buzz"]
fizzbuzz n = fizzbuzz(n - 1) ++ [show n]

-- the above vertical bars are refered to as guard

main :: IO ()
main = do
  putStrLn "hello"
  print (fizzbuzz 100)
  
  
