module Days.Day11 (runDay) where
    
{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Test.HUnit hiding (State, test)
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Control.Monad.State.Strict
import Data.Ord
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 monkey (string "\n\n")

pnumber :: Parser Int
pnumber = read <$> many1 digit

monkey :: Parser Monkey
monkey =
  Monkey 0
    <$> (string "Monkey " *> pnumber <* string ":\n")
    <*> (string "  Starting items: " *> (Seq.fromList <$> sepBy1 (toEnum <$> pnumber) (string ", ")) <* char '\n')
    <*> (string "  Operation: new = " *> exprParse <* char '\n')
    <*> (string "  Test: divisible by " *> (toEnum <$> pnumber) <* char '\n')
    <*> (string "    If true: throw to monkey " *> pnumber <* char '\n')
    <*> (string "    If false: throw to monkey " *> pnumber)

exprParse :: Parser Expr
exprParse = (\v o v' -> o v v') <$> (valueParse <* char ' ') <*> (operator <* char ' ') <*> valueParse

operator :: Parser (Value -> Value -> Expr)
operator = (char '+' $> Plus) <|> (char '*' $> Times)

valueParse :: Parser Value
valueParse = (string "old" $> OldValue) <|> (Number <$> (toEnum <$> pnumber))

------------ TYPES ------------
type Input = [Monkey]

data Monkey = Monkey {inspected :: Int, number :: Int, items :: Seq Int, operation :: Expr, test :: Int, trueCond :: Int, falseCond :: Int} deriving (Show)

data Expr = Times Value Value | Plus Value Value deriving (Show)

data Value = Number Int | OldValue deriving (Show)



type OutputA = Int

type OutputB = Int

------------ PART A ------------

runValue :: Int -> Value -> Int
runValue n (Number i) = i
runValue n OldValue = n

runExpr :: Int -> Int -> Expr -> Int
runExpr l n (Times va va') = (runValue n va * runValue n va') `mod` l
runExpr l n (Plus va va') = (runValue n va + runValue n va') `mod` l

runMonkey :: Int -> (Int -> Int) -> Monkey -> (Monkey, [(Int, Int)])
runMonkey l worryEffect (Monkey ins i ns ex cond t f) = (Monkey (ins + length ns) i Seq.empty ex cond t f, map updatePos (toList ns))
  where
    updatePos :: Int -> (Int,Int)
    updatePos v = (if v' `mod` cond == 0 then t else f, v')
      where
        v' = worryEffect (runExpr l v ex)

runRound :: Int -> (Int -> Int) -> State (Vector Monkey) ()
runRound l w =
  do i <- gets length
     forM_ [0..i-1] $ \ind ->
       do m <- gets (Vec.! ind)
          let (newMonk,throwList) = runMonkey l w m
          modify $ \s -> Vec.accum (\m item -> m {items = items m Seq.|> item}) (s Vec.// [(ind,newMonk)]) throwList


sortDes :: Ord a => [a] -> [a]
sortDes = map getDown . sort . map Down

partA :: Input -> OutputA
partA mons = a * b
  where
    vec = Vec.fromList mons
    l = foldl1 lcm $ map test mons
    final = execState (replicateM 20 (runRound l (`quot` 3))) vec
    (a:b:_) = sortDes $ map inspected $ toList final

------------ PART B ------------
partB :: Input -> OutputB
partB mons = a * b
  where
    vec = Vec.fromList mons
    final = execState (replicateM 10000 (runRound l id)) vec
    l = foldl1 lcm $ map test mons
    (a:b:_) = sortDes $ map inspected $ toList final

------------ Tests  ------------

testInput = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1\n"

tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testInput 10605,
                  TestCase $ U.inputTest inputParser partB testInput 2713310158]
