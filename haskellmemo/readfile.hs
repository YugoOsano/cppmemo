{-
types
readFile :: FilePath -> IO String
putStrLn :: String -> IO ()
-}
{-
main = do
  text <- readFile "sample.csv"
  print text
-}
main = 
  readFile "sample.csv" >>= putStrLn
  
