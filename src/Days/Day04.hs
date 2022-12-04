module Days.Day04 (runDay) where

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
import Util.Util
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 ((,) <$> (range <* char ',') <*> range <* char '\n')

range :: Parser (Int, Int)
range = (,) <$> (read <$> many1 digit) <*> (char '-' *> (read <$> many1 digit))

------------ TYPES ------------
type Input = [((Int,Int),(Int,Int))]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter eclipsed

eclipsed :: ((Int, Int), (Int, Int)) -> Bool
eclipsed ((n, i), (j, x)) = (n <= j && i >= x) || (n >= j && i <= x)


------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter overlap

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((n, i), (j, x)) = i >= j && n <= x || x >= n && j <= i


------------ Tests  ------------

testData = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\n"

tests :: Test
tests = TestList [ "part a test" ~: TestCase (inputTest inputParser partA testData 2),
                   "partB test" ~: TestCase (inputTest inputParser partB testData 4)]
