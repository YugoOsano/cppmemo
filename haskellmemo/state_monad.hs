-- Haskell Nyuumon: State Monad
-- run in ghci by:

-- :load state_monad.hs
-- St.runState gameWithState [1..50]

-- The output will be:
-- ([(90,[16,17,18,19,20],"Yumi"),(65,[11,12,13,14,15],"Takashi"),(40,[6,7,8,9,10],"Hanako"),(15,[1,2,3,4,5],"Taro")],[21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50])



--import Control.Monad.State
import qualified Control.Monad.Trans.State as St
import qualified Data.List                 as Ls

-- a :: State s Int
-- a = return 1

-- main = return ()

-- main = do
--  let a = return 1 :: State s Int
--  print $ runState a () -- () is the initial state (mandatory)

type Card  = Int
type Score = Int
type Hand  = [Card]
type Stock = [Card]
type Player = String

drawCards :: Int -> St.State Stock Hand
drawCards n = do
  deck <- St.get
  St.put $ Ls.drop n deck
  return $ Ls.take n deck

gameWithState = do
  -- delivery of cards
  taroHand    <- drawCards 5
  hanakoHand  <- drawCards 5
  takashiHand <- drawCards 5
  yumiHand    <- drawCards 5

  return . Ls.reverse . Ls.sort $
    [(Ls.sum taroHand,    taroHand,    "Taro")
    ,(Ls.sum hanakoHand,  hanakoHand,  "Hanako")
    ,(Ls.sum takashiHand, takashiHand, "Takashi")
    ,(Ls.sum yumiHand,    yumiHand,    "Yumi")
    ]
  

--main = St.runState gameWithState [1..50]

  
