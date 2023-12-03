module Days.Day13 (runDay) where
    
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
import Test.HUnit
import Control.Applicative (Alternative((<|>)))
import Debug.Trace (trace)
-- import Data.Functor.Foldable
-- import Data.Functor.Foldable.TH

{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 pair (string "\n\n")

pair :: Parser (NestedList Int,NestedList Int)
pair = (,) <$> (list <* char '\n') <*> list

list :: Parser (NestedList Int)
list = (End . read <$> many1 digit) <|> SubList <$> (char '[' *> sepBy' list (char ',') <* char ']')

------------ TYPES ------------
type Input = [(NestedList Int,NestedList Int)]

data NestedList a = SubList [NestedList a] | End a deriving (Functor,Show,Eq)

data These a b = This a | That b | Both a b

comp :: Ord a => NestedList a -> NestedList a -> Ordering -- could be done with recursion schemes but would be vary verbose
comp (SubList []) (SubList []) = EQ
comp (SubList []) (SubList (nl : nls)) = LT
comp (SubList (nl : nl_as)) (SubList []) = GT
comp (SubList (nl : nl_as)) (SubList (nl' : nls)) = case nl `comp` nl' of
    EQ -> SubList nl_as `comp` SubList nls
    a -> a
comp l@(SubList nls) (End a) = l `comp` SubList [End a]
comp (End a) l@(SubList nls) = SubList [End a] `comp` l
comp (End a) (End a') = a `compare` a'

type OutputA = Int

type OutputB = Int

------------ PART A ------------
rightOrder :: Ord a => NestedList a -> NestedList a -> Bool
rightOrder a b = (==LT) $ comp a b

partA :: Input -> OutputA
partA i = sum $ map fst $ filter snd $ zip [1..] $ map (uncurry rightOrder) i

------------ PART B ------------
partB :: Input -> OutputB
partB i = l1 * l2
    where div1 = SubList [SubList [End 2]]
          div2 = SubList [SubList [End 6]]
          [l1,l2] = map fst
                  $ filter (\(_,a) -> a == div1 || a == div2)
                  $ zip [1..]
                  $ sortBy comp (div1 : div2 : i' )
          i' = concatMap (\(a,b) -> [a,b]) i



------------ Tests  ------------

testInput = "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"

tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testInput 13,
                  TestCase $ U.inputTest inputParser partB testInput 140]
