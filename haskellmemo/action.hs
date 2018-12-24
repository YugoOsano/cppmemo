import qualified System.Random as R

randAlpha = R.getStdRandom $ R.randomR('A', 'Z')

main = do
  -- <- retrieves a value from an action in do
  --   (to store it to a variable)
  r <- randAlpha
  print r

  -- =<< retrieves a value and transfers it to a function 
  print =<< randAlpha

  -- orthodox bind
  randAlpha >>= print
  
