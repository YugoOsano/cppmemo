data Employee = Employee
  {
    name :: String,
    age  :: Int,
    role :: Role
  }
data Role = Engineers | Sales | Designers

outputNumber :: Role -> Int
outputNumber (Engineers)
  = 1 
outputNumber (Sales)
  = 2 
outputNumber (Designers)
  = 3 
------
seniorEmployee :: Employee -> Role
seniorEmployee (Employee name age role)
  = Engineers
-----
-- ad-hoc polymorphism
-- see: http://www.nslabs.jp/haskell-poly.rhtml
--
-- Updater:      the name of type class
-- genericValue: the name of type variable
-- multiple functions can be defined (but only 'update' is defined here) 
class Updater genericValue where
  update :: genericValue -> genericValue  
  update    genericValue =  genericValue

-- the type variable above takes Int here
instance Updater Int where
  update    int = int + 1

f :: (Updater a) => a -> a   
f x = update x -- ++ " = " ++ (show x)

-- compile: ghc polymorphism.hs 
main = do
  putStrLn "hello, world!"

  print $ outputNumber Engineers
  print $ outputNumber Sales
  print $ outputNumber Designers

