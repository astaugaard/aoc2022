module Days.Day03 (runDay) where

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
import Data.Char
import Debug.Trace
import Util.Util (inputTest)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' ((\l -> let (a,b) = splitAt ((length l + 1) `div` 2) l in (Set.fromList a,Set.fromList b)) <$> many' (satisfy (/='\n')) <* char '\n')

------------ TYPES ------------
type Input = [(Set Char,Set Char)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

weight = (\c -> if isUpper c then ord c - ord 'A' + 27 else ord c - ord 'a' + 1)

partA :: Input -> OutputA

partA = sum . map (\(a,b) -> sum $ map weight $ Set.toList $ Set.intersection a b)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . concatMap (\l -> map weight . Set.toList . foldl1 Set.intersection $ map (\(a,b) -> Set.union a b) l) . U.chunksOf 3


------------ Tests  ------------

testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"

tests :: Test
tests = TestList ["part a test" ~: TestCase (inputTest inputParser partA testInput 157),
                  "partb test" ~: TestCase (inputTest inputParser partB testInput 70)]
