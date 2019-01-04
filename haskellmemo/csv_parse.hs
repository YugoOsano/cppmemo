-- source code comes from:
-- http://book.realworldhaskell.org/read/using-parsec.html
-- file: ch16/csv1.hs

-- Japanese remarks:
-- https://qiita.com/rooooomania/items/54bc0e0b725d04c119c7

-- debug in Haskell:
-- https://blog.miz-ar.info/2018/01/debugging-haskell-program/

-- to use in ghci:
-- :load csv_parse
-- parseCSV ""
-- parseCSV "abc,def,ghi\n"

import qualified Text.ParserCombinators.Parsec as PC
import qualified Data.Either as EI

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
{- type of PC.many:
   Text.Parsec.Prim.ParsecT s u m a
   -> Text.Parsec.Prim.ParsecT s u m [a]

   st is likely to stand for: state transformer
   according to
   https://qiita.com/7shi/items/2e9bff5d88302de1a9e9
-}
csvFile :: PC.GenParser Char st [[String]]
csvFile = 
    do result <- PC.many line
       PC.eof
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: PC.GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)
       
-- Each line contains 1 or more cells, separated by a comma
line :: PC.GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: PC.GenParser Char st [String]
remainingCells =
    (PC.char ',' >> cells)            -- Found comma?  More cells coming
    PC.<|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: PC.GenParser Char st String
cellContent = 
    PC.many (PC.noneOf ",\n")
       

-- The end of line character is \n
eol :: PC.GenParser Char st Char
eol = PC.char '\n'

{-
 type of PC.parse is:
 Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t =>
     Text.Parsec.Prim.Parsec s () a
     -> PC.SourceName -> s -> Either PC.ParseError a
-}
parseCSV :: String -> Either PC.ParseError [[String]]
parseCSV input = PC.parse csvFile "unknown" input
       
main = do 
  allstringsinfile <- readFile "sample.csv"
  print $ show (parseCSV allstringsinfile) 

  -- show function converts every Show class' instance to a string
  
  --print =<< parseCSV =<< readFile "sample.csv"
  --parseCSV text
