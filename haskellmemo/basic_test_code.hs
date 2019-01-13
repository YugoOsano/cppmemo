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

-- pattern match
-- specific args need to come ahead
func 1 = 1111
func x = x

funcGuard x 
  | x == 1  = 976
  | x >  1  = x
  | otherwise = 1976

{- create my own higher order function
   here, the type is automatically deducted as
   (Enum t1, Num t1) => (t1 -> t) -> t1 -> [t]
-}
higherOrder f x =
  [f elem | elem <- [x..(x+2)]]


{-
 what's equivalent to following in C:
  for (int i=0; i<10; i++) {
    b[i] = i + a[i];
  }
  use of elements and indices both needs to be
  taken as a pair of lists;

  or to make it better:
  https://stackoverflow.com/questions/6472883/using-list-elements-and-indices-together
-}
forWithEnumerateSubstitute :: [Int] -> [Int] -> [Int]
forWithEnumerateSubstitute xs indices = 
  [ fst(x_index) + snd(x_index) |
    x_index <- (zip xs indices)] :: [Int]
  
-- composite function
compositeIncr ::  Int -> Int
compositeIncr x = y
  where tmp1 = incr_custom x
        tmp2 = incr_custom tmp1
        -- logic can be nested by a lambda
        -- ('where' can't be used therein)
        y    = (\v ->
                 let tmp_inner = incr_custom v
                     w         = incr_custom tmp_inner
                 in w) tmp2

-- let version
-- (let is an expression, can be located anywhere:
--  see https://qiita.com/YusukeHosonuma/items/5da9847db16d33f27a06)
compositeIncrLet :: Int -> Int
compositeIncrLet x =
  let tmp1 = incr_custom x
      tmp2 = incr_custom tmp1
      y    = incr_custom tmp2
    in y

-- a tuple is defined by simple paranthesis
-- type of foldr is: Foldable t => (a->b->b) -> b-> t a -> b
sumAndAverage :: [Double] -> (Double, Double)
sumAndAverage xs = (sum, ave)
  where sum = foldr (+) 0 xs
        ave = sum / (fromIntegral (length xs))
        -- type of fromIntegral's return is deducted (as Double)

main :: IO()
main = do
  -- local variable definition
  -- (without the above do, the following print z
  --  would be  [in print z]) 
  let x = 1
      y = 2
      z = x + y
  print z

  print $ func 2
  print $ func 1
  print $ funcGuard 2
  print $ funcGuard 1
  print $ funcGuard 0
  {-
   (range-based) for loop in Python can be covered
   by map with a list or list comprehension
  -}
  print $ map funcGuard [0..9]
  print [funcGuard x | x <- [0..9]]
  print $ forWithEnumerateSubstitute [0,1,2] [0, 100, 200]

  print $ compositeIncr    101
  print $ compositeIncrLet 101

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

  print $ sumAndAverage [1,2,3,4,5]

  -- another style of local variable
  print w
  where x = 1
        y = 2
        w = x + y

