
import qualified Data.List as L
import Control.Monad.Trans.State
import Control.Monad.Loops


data Register = RegA | RegB
                deriving (Show)

type Offset = Int

data Instruction = Hlf Register
                 | Tpl Register
                 | Inc Register
                 | Jmp Offset
                 | Jie Register Offset
                 | Jio Register Offset
                 deriving (Show)

type Instructions = [Instruction]

data MacState = MacState { ip :: Int
                         , regA :: Int
                         , regB :: Int
                         , ins :: Instructions
                         , lins :: Int}
                         deriving (Show)


type MacS = State MacState


parseOffset :: String -> Int
parseOffset ('+':ns) = read ns
parseOffset ('-':ns) = -read ns
parseOffset ns = read ns


parseInstruction :: [String] -> Instruction
parseInstruction ["hlf", r]    = Hlf $ if r == "a" then RegA else RegB
parseInstruction ["tpl", r]    = Tpl $ if r == "a" then RegA else RegB
parseInstruction ["inc", r]    = Inc $ if r == "a" then RegA else RegB
parseInstruction ["jmp", o]    = Jmp $ parseOffset o
parseInstruction ["jie", r, o] = Jie (if r!!0=='a' then RegA else RegB)
                                     (parseOffset o)
parseInstruction ["jio", r, o] = Jio (if r!!0=='a' then RegA else RegB)
                                     (parseOffset o)


parseInstructions :: String -> Instructions
parseInstructions = map parseInstruction . map words . lines

end :: MacS Bool
end = do s <- get
         return $ 0 <= ip s && ip s < lins s

exeIns :: MacS ()
exeIns = do s <- get
            let i = ip s
            case (ins s)!!i of
              Hlf RegA -> put $ s{ip=i+1, regA=regA s `div` 2}
              Hlf RegB -> put $ s{ip=i+1, regB=regB s `div` 2}
              Tpl RegA -> put $ s{ip=i+1, regA=regA s*3}
              Tpl RegB -> put $ s{ip=i+1, regB=regB s*3}
              Inc RegA -> put $ s{ip=i+1, regA=regA s+1}
              Inc RegB -> put $ s{ip=i+1, regB=regB s+1}
              Jmp o -> put $ s{ip=i+o}
              Jie RegA o -> put $ s{ip=if regA s `mod` 2==0 then i+o else i+1}
              Jie RegB o -> put $ s{ip=if regB s `mod` 2==0 then i+o else i+1}
              Jio RegA o -> put $ s{ip=if regA s==1 then i+o else i+1}
              Jio RegB o -> put $ s{ip=if regB s==1 then i+o else i+1}


run :: MacS Int
run = do whileM_ end exeIns
         s <- get
         return $ regB s

main = do input <-readFile "input.txt"
          let is = parseInstructions input
          let state = MacState {ip=0, regA=0, regB=0, ins=is, lins=length is}
          let val = evalState run state
          print val
