-- this file comes from:
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

{-# START_FILE Main.hs #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative
-- We import ByteString qualified because the function
-- 'Data.ByteString.readFile' would clash with
-- 'Prelude.readFile'.
import qualified Data.ByteString as B

-----------------------
------ SETTINGS -------
-----------------------

-- | File where the log is stored.
logFile :: FilePath
logFile = "sellings.log"

-----------------------
-------- TYPES --------
-----------------------

-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

-- | Type for log entries.
--   Add, remove of modify fields to fit your own log file.
data LogEntry =
  LogEntry { entryTime :: LocalTime
           , entryIP   :: IP
           , entryProduct   :: Product
             } deriving Show

type Log = [LogEntry]

-----------------------
------- PARSING -------
-----------------------

-- | Parser of values of type 'IP'.
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

-- | Parser of values of type 'LocalTime'.
timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }

-- | Parser of values of type 'Product'.
productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)

-- | Parser of log entries.
logEntryParser :: Parser LogEntry
logEntryParser = do
  -- First, we read the time.
  t <- timeParser
  -- Followed by a space.
  char ' '
  -- And then the IP of the client.
  ip <- parseIP
  -- Followed by another space.
  char ' '
  -- Finally, we read the type of product.
  p <- productParser
  -- And we return the result as a value of type 'LogEntry'.
  return $ LogEntry t ip p

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

----------------------
-------- MAIN --------
----------------------

main :: IO ()
main = B.readFile logFile >>= print . parseOnly logParser
