
import Prelude hiding (round)

import qualified Data.List as L
import Control.Monad.Trans.State
import Control.Monad.Loops


data Actor = Boss | Player
             deriving (Show)

data Item = Weapon {itemCost :: Int, itemDamage :: Int, itemArmor :: Int}
          | Armor {itemCost :: Int, itemDamage :: Int, itemArmor :: Int}
          | Ring {itemCost :: Int, itemDamage :: Int, itemArmor :: Int}
          deriving (Show)

dagger     = Weapon {itemCost=8, itemDamage=4, itemArmor=0}
shortsword = Weapon {itemCost=10, itemDamage=5, itemArmor=0}
warhammer  = Weapon {itemCost=25, itemDamage=6, itemArmor=0}
longsword  = Weapon {itemCost=40, itemDamage=7, itemArmor=0}
greataxe   = Weapon {itemCost=74, itemDamage=8, itemArmor=0}

leather    = Armor {itemCost=13, itemDamage=0, itemArmor=1}
chainmail  = Armor {itemCost=31, itemDamage=0, itemArmor=2}
splintmail = Armor {itemCost=53, itemDamage=0, itemArmor=3}
bandedmail = Armor {itemCost=75, itemDamage=0, itemArmor=4}
platemail  = Armor {itemCost=102, itemDamage=0, itemArmor=5}

ringDamage1  = Ring {itemCost=25, itemDamage=1, itemArmor=0}
ringDamage2  = Ring {itemCost=50, itemDamage=2, itemArmor=0}
ringDamage3  = Ring {itemCost=100, itemDamage=3, itemArmor=0}
ringDefense1 = Ring {itemCost=20, itemDamage=0, itemArmor=1}
ringDefense2 = Ring {itemCost=40, itemDamage=0, itemArmor=2}
ringDefense3 = Ring {itemCost=80, itemDamage=0, itemArmor=3}

weapons = [dagger, shortsword, warhammer, longsword, greataxe]
armor = [leather, chainmail, splintmail, bandedmail, platemail]
rings = [ringDamage1, ringDamage2, ringDamage3,
         ringDefense1, ringDefense2, ringDefense3]


data ActorState = ActorState { stateHit :: Int
                             , stateDamage :: Int
                             , stateArmor :: Int
                             , gold :: Int}
                             deriving (Show)

data RoundState = RoundState { gameBossState :: ActorState
                             , gamePlayerState :: ActorState
                             , next :: Actor}
                             deriving (Show)


type GameS = State RoundState

playerHit :: Int
playerHit = 100

twoSets :: [a] -> [[a]]
twoSets [] = []
twoSets [_] = []
twoSets (x:xs) = map (:[x]) xs ++ twoSets xs

addItemData :: Item -> ActorState -> ActorState
addItemData item actor =
  actor {stateDamage=stateDamage actor + itemDamage item,
         stateArmor=stateArmor actor + itemArmor item,
         gold=gold actor + itemCost item}

extractItemsData :: [Item] -> ActorState
extractItemsData =
  foldr addItemData  actor
  where actor = ActorState {stateHit=playerHit, stateDamage=0,
                            stateArmor=0, gold=0}

playerGenStates :: [ActorState]
playerGenStates =
  L.sortBy (\a b -> gold a `compare` gold b) $ map extractItemsData itemSets
  where armorSets = []:map (:[]) armor
        ringSets = []:map (:[]) rings ++ twoSets rings
        itemSets = [w:(as++rs) | w <- weapons, as <- armorSets, rs <- ringSets]

parseBossInitState :: String -> ActorState
parseBossInitState str =
  ActorState {stateHit=ns!!0, stateDamage=ns!!1, stateArmor=ns!!2, gold=0} 
  where ns = map (read . last . words) $ lines str
        

-- Game specialized functions

exitRound :: GameS Bool
exitRound = do s <- get
               let hb = gameBossState s
                   hp = gamePlayerState s
               return $ stateHit hp <= 0 || stateHit hb <= 0

turnBoss :: GameS ()
turnBoss = do s <- get
              let hb = gameBossState s
                  hp = gamePlayerState s
                  damag = max (stateDamage hb-stateArmor hp) 1
                  hit = max (stateHit hp-damag) 0
              put $ s {gamePlayerState = hp{stateHit=hit}, next=Player}

turnPlayer :: GameS ()
turnPlayer = do s <- get
                let hb = gameBossState s
                    hp = gamePlayerState s
                    damag = max (stateDamage hp-stateArmor hb) 1
                    hit = max (stateHit hb-damag) 0
                put $ s {gameBossState = hb{stateHit=hit}, next=Boss}


turn :: GameS ()
turn = do s <- get
          case next s of
            Boss -> turnBoss  
            Player -> turnPlayer
          


round :: ActorState -> ActorState -> GameS Bool
round boss player = do put $ RoundState boss player Player
                       untilM_ turn exitRound
                       s <- get
                       return $ (stateHit (gamePlayerState s) > 0)

roundSeq :: ActorState -> GameS () 
roundSeq boss = do firstM (round boss) playerGenStates
                   return ()

main = do input <-readFile "input.txt"
          let bossState = parseBossInitState input
          let state = execState (roundSeq bossState) undefined
          print $ gold $ gamePlayerState state
