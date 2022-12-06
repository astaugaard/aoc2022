
module Days.Day06 (runDay) where
        
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





import qualified Data.List.NonEmpty as N
import Control.Comonad
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (N.:|) <$> anyChar <*> many' anyChar

------------ TYPES ------------
type Input = N.NonEmpty Char

type OutputA = Int

type OutputB = Int

------------ PART A ------------

firstNDifferent :: Eq a => Int -> N.NonEmpty a -> Bool
firstNDifferent n non = (length $ nub $ N.take n non) == n

first4Different :: Eq a => N.NonEmpty a -> Bool
first4Different = firstNDifferent 4

partA :: Input -> OutputA
partA = (+4) . length . N.takeWhile not . extend first4Different

------------ PART B ------------
partB :: Input -> OutputB
partB = (+14) . length . N.takeWhile not . extend (firstNDifferent 14)

------------ Tests  ------------
testInputs = ["mjqjpqmgbljsphdztnvjfqwrcgsmlb","bvwbjplbgvbhsrlpgdmjqwftvncz","nppdvjthqldpwncqszvftbrmjlhg","nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg","zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]

tests :: Test
tests = TestList $ zipWith (\t v -> TestCase $ U.inputTest inputParser partA t v) testInputs [7,5,6,10,11] ++ zipWith (\t v -> TestCase $ U.inputTest inputParser partB t v) testInputs [19,23,23,29,26]
