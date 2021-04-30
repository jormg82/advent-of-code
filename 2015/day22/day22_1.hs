
import Control.Monad.Trans.State
import System.IO.Unsafe


data Spell = Missile | Drain | Shield | Poison | Recharge
             deriving (Enum, Show)


data GameState = GameState { bossHit :: Int
                           , bossDamage :: Int
                           , playerHit :: Int
                           , playerMana :: Int
                           , spentMana :: Int
                           , shieldTimer :: Int
                           , poisonTimer :: Int
                           , rechargeTimer :: Int}
                           deriving (Show)

data SimuState = SimuState { gameStack :: [GameState]
                           , minMana :: Int}
                           deriving (Show)

type GameS = State SimuState

data TurnResult = PlayerWins | Continue | Abort
                  deriving (Eq, Show)

initPlayerHit, initPlayerMana :: Int
initPlayerHit = 50
initPlayerMana = 500
--initPlayerHit = 10
--initPlayerMana = 250


parseBossInitData :: String -> (Int, Int)
parseBossInitData str =
  (ns!!0, ns!!1)
  where ns = map (read . last . words) $ lines str



------------------------------------------

applyPoison :: GameS ()
applyPoison =
  do gState <- getGState
     let bh = max (bossHit gState-3) 0
     if poisonTimer gState > 0 then
       updateGState $ gState {bossHit=bh}
     else
       return ()

applyRecharge :: GameS ()
applyRecharge =
  do gState <- getGState
     let pm = playerMana gState + 101
     if rechargeTimer gState > 0 then
       updateGState $ gState {playerMana=pm}
     else
       return ()


applyEffects :: GameS TurnResult
applyEffects =
  do applyPoison >> applyRecharge     
     bh <- get >>= return . bossHit . head . gameStack
     if bh <= 0 then
       return PlayerWins
     else
       return Continue


turnPlayer :: Spell -> GameS TurnResult
turnPlayer Missile  = turnMissile
turnPlayer Drain    = turnDrain
turnPlayer Shield   = turnShield
turnPlayer Poison   = turnPoison
turnPlayer Recharge = turnRecharge


     
turnMissile :: GameS TurnResult
turnMissile =
  do gState <- getGState
     let bh = max (bossHit gState-4) 0
         pm = playerMana gState - 53
         sm = spentMana gState + 53

     if pm >= 0 then
       do updateGState $ gState {bossHit=bh,
                                 playerMana=pm,
                                 spentMana=sm}
          if bh > 0 then
            return Continue
          else
            return PlayerWins
     else
       return Abort


turnDrain :: GameS TurnResult
turnDrain =
  do gState <- getGState
     let bh = max (bossHit gState-2) 0
         ph = playerHit gState + 2
         pm = playerMana gState - 73
         sm = spentMana gState + 73

     if pm >= 0 then
       do updateGState $ gState {bossHit=bh,
                                 playerHit=ph,
                                 playerMana=pm,
                                 spentMana=sm}
          if bh > 0 then
            return Continue
          else
            return PlayerWins
     else
       return Abort


turnShield :: GameS TurnResult
turnShield =
  do gState <- getGState
     let pm = playerMana gState - 113
         sm = spentMana gState + 113
         st = shieldTimer gState
     if st <= 1 && pm >= 0 then
       do updateGState $ gState {shieldTimer=6,
                                 playerMana=pm,
                                 spentMana=sm}
          return Continue
     else
       return Abort


turnPoison :: GameS TurnResult
turnPoison =
  do gState <- getGState
     let pm = playerMana gState - 173
         sm = spentMana gState + 173
         st = poisonTimer gState
     if st <= 1 && pm >= 0 then
       do updateGState $ gState {poisonTimer=6,
                                 playerMana=pm,
                                 spentMana=sm}
          return Continue
     else
       return Abort

turnRecharge :: GameS TurnResult
turnRecharge =
  do gState <- getGState
     let pm = playerMana gState - 229
         sm = spentMana gState + 229
         st = rechargeTimer gState
     if st <= 1 && pm >= 0 then
       do updateGState $ gState {rechargeTimer=5,
                                 playerMana=pm,
                                 spentMana=sm}
          return Continue
     else
       return Abort


getGState :: GameS GameState
getGState =  get >>= return . head . gameStack


updateGState :: GameState -> GameS ()
updateGState gState = do s <- get
                         let gs = tail $ gameStack s
                         put $ s {gameStack=gState:gs}
 
dupGState :: GameS ()
dupGState = do s <- get
               let gs = gameStack s
               put $ s {gameStack=head gs:gs}


popGState :: GameS ()
popGState = do s <- get
               let gs = gameStack s
               put $ s {gameStack=tail gs}


haltOn :: Monad m => (a -> Bool) -> [m a] -> m a
haltOn p [x] = x
haltOn p (x:xs) = x >>= (\r -> if p r then return r else haltOn p xs)
                    


turnBoss :: GameS TurnResult
turnBoss =
  do gState <- get >>= return . head . gameStack
     let shield = if shieldTimer gState > 0 then 7 else 0
         ph = max (playerHit gState - (max (8-shield) 1)) 0
     updateGState $ gState {playerHit=ph}
     if ph > 0 then 
       return Continue
     else
       return Abort


decTimers :: GameS TurnResult
decTimers =
  do gState <- get >>= return . head . gameStack
     let st = max (shieldTimer gState-1) 0
         pt = max (poisonTimer gState-1) 0
         rt = max (rechargeTimer gState-1) 0
     updateGState $ gState {shieldTimer=st,
                            poisonTimer=pt,
                            rechargeTimer=rt}
     return Continue


doubleTurn :: Spell -> GameS ()
doubleTurn spell =
  do dupGState
     r <- haltOn (\r -> r == PlayerWins || r == Abort)
                 [applyEffects, decTimers, turnPlayer spell,
                  applyEffects, decTimers, turnBoss]

     case r of
       PlayerWins -> do s <- get
                        gState <- getGState
                        let mm = minMana s
                            sm = spentMana gState
                        (unsafePerformIO (if sm < mm then print sm else return ())) `seq` (put $ s {minMana=min mm sm})
       Continue   -> simuSeq
       Abort      -> return ()

     popGState


simuSeq :: GameS ()
simuSeq = sequence_ $ map doubleTurn [Recharge .. Missile]


main = do input <- readFile "input.txt"
          let (bossH, bossD) = parseBossInitData input
              gameState = GameState {bossHit=bossH,
                                     bossDamage=bossD,
                                     playerHit=initPlayerHit,
                                     playerMana=initPlayerMana,
                                     spentMana=0,
                                     shieldTimer=0,
                                     poisonTimer=0,
                                     rechargeTimer=0}
              simuState = SimuState {gameStack=[gameState],
                                     minMana=maxBound::Int}
              result = minMana $ execState simuSeq simuState
          print result
