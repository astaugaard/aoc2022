module Days.Day01 (runDay) where

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
import Data.Ord
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy elf (char '\n' <* char '\n')

elf :: Parser [Int]
elf = sepBy (read <$> many1 digit) (char '\n')

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA i = maximum $ map sum i

------------ PART B ------------
sortDes :: Ord a => [a] -> [a]
sortDes = map getDown . sort . map Down

partB :: Input -> OutputB
partB = sum . Data.List.take 3 . sortDes . map sum


------------ Tests  ------------
tests :: Test
tests = TestList []
