-- reference is
-- https://www.slideshare.net/xenophobia__/ss-14558187

-- for the first use of vector, run:
-- stack ghci --package vector

import qualified Data.Array as Array
import qualified Data.Vector as V

main = do
  let a = Array.listArray (0,5)['a'..]
  print a
  print $ a Array.! 2

  let b = a Array.// [(2,'x'),(3,'y')]
  print b

  -- index range can be arbitrary
  let c = Array.listArray (10,15)['a'..]
  print c

  -- vector
  let animals = V.fromList["Dog", "Pig", "Cat", "Fox"]
  print animals
  
