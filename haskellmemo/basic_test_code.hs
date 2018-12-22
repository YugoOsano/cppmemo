-- run by:
-- (1) ghc basic_test_code.hs
--     ./basic_test_code
-- (2) stack runghc -- basic_test_code.hs

-- variable declaration at top level
a = 2
-- let y = 10 in y+10 :: Int
-- An error of 'naked expression at top level' comes up 

-- type annotation (Kata Chushaku)
-- if we let the function accept any type,
-- we need to skip the type annotation and the last (::Int)
incr_custom :: Int -> Int
incr_custom n = n+1 :: Int

-- An example of non strict function in Haskell Nyumon
hf :: Int -> Int
hf x = 1

-- https://qiita.com/7shi/items/1ce76bde464b4a55c143
-- data type must start by an Uppercase
--  sentense structure in type of direct product:
--   data [type] = [constructor] [field...]
--    ([type] and [constructor] can be a same name)
data ShowableInt = ShowableInt Int deriving (Show)

funcDeriveShow :: ShowableInt -> ShowableInt
funcDeriveShow x = x 

-- to enforce evalution
hf_enforced_eval :: Int -> Int
hf_enforced_eval x = seq x 1 

main :: IO()
main = do
  let x = 1
  -- x = x + 1
  putStrLn "Hello, World"
  -- only rhs references variables which follow where
  -- print (y + (incr_custom z))
  --  where {y=10; z=20}

  -- if the type annotation was skipped on incr_custom,
  -- incr_custom could accept a float value.

  -- 0/0 turing to NaN value would result in an error in other languages
  -- (with strict function)
  print (hf (0 `div` 0))

  -- print (hf_enforced_eval (0 `div` 0)) -- error (divide by zero)

  -- in ghci, :t print results in a type including =>.
  -- this means a typeclass constraint (see stackoverflow 9142731).
  
  print (ShowableInt 3)
  print $ funcDeriveShow $ ShowableInt 4
