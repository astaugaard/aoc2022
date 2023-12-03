module Days.Day15 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 (string "Sensor at " *> ((,) <$> loc <*> (string ": closest beacon is at " *> loc))) (char '\n')

num :: Parser Int
num = read <$> many1 (satisfy (\c -> isDigit c || c == '-'))

loc :: Parser Loc
loc = (,) <$> (string "x=" *> num) <*> (string ", y=" *> num)

------------ TYPES ------------
type Input = [(Loc,Loc)]

type Loc = (Int,Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------

type ValidRangeList a = [(a,a)]

subRange :: (Ord a,Enum a) => ValidRangeList a -> (a,a) -> ValidRangeList a
subRange rl s = do sv <- rl
                   splitRange s sv

splitRange :: (Ord a,Enum a) => (a,a) -> (a, a) -> ValidRangeList a
splitRange a@(a1, a2) (b1, b2) | b2 <= a2 && a1 <= b1 = []
                               | a1 > b1 && a2 < b2 = [(b1,pred a1),(succ a2,b2)]
                               | a1 <= b1 && a2 >= b1 = [(succ a2,b2)]
                               | a2 >= b2 && a1 <= b2 = [(b1,pred a1)]
                               | otherwise = [(b1,b2)]

computeRangeInRow :: Loc -> Int -> Int -> Maybe (Int,Int)
computeRangeInRow (x, y) range row | diff > range = Nothing
                                   | otherwise = Just (x - range + diff, x + range - diff)
  where diff = abs $ row - y

inRange :: Loc -> Int -> Loc -> Bool
inRange (sensX,sensY) range (sqX,sqY) = range >= (abs (sensX - sqX) + abs (sensY - sqY))

calculateRange :: Loc -> Loc -> Int
calculateRange s@(sx,sy) b@(bx,by) = abs (sx - bx) + abs (sy - by)

-- openInRow :: Loc -> Loc -> Int -> [(Loc,Int)] -> Set Loc -> Int
-- openInRow (x,y) (x',y') row ranges known = length $ filter (\l -> not (Set.member (l,row) known) && any (\(s,r) -> inRange s r (l,row)) ranges) [x..x']

openInRow :: Loc -> Loc -> Int -> [(Loc,Int)] -> [Loc] -> Int
openInRow (minx,_) (maxx,_) row beacRanges known = ((maxx - minx)-)
                                                 $ sum
                                                 $ map (\(lx,hx) -> hx - lx + 1)
                                                 $ foldl' (\rRow (loc,range) -> subMaybeRange rRow (computeRangeInRow loc range row))
                                                    [(minx,maxx)] beacRanges

subMaybeRange :: ValidRangeList Int -> Maybe (Int, Int) -> ValidRangeList Int
subMaybeRange x0 Nothing = x0
subMaybeRange x0 (Just x1) = subRange x0 x1

openInRow' :: Int -> [(Loc,Loc)] -> Int
-- minv = upper left hand corner
-- maxv = lower right hand corner
openInRow' r i = openInRow (getMinV sAndR) (getMaxV sAndR) r sAndR (concatMap (\(l1,l2) -> [l1,l2]) i)
    where sAndR = map (\(sens,beac) -> (sens,calculateRange sens beac)) i

get__V :: (Int -> Int -> Int) -> ([Int] -> Int) -> [(Loc,Int)] -> Loc
get__V dist comp locs = (comp $ map fst locs',comp $ map snd locs')
    where locs' = map (\(l1@(x,y),r) -> (dist x r,dist y r)) locs

getMinV = get__V (-) minimum
getMaxV = get__V (+) maximum

partA :: Input -> OutputA
partA = openInRow' 2000000 -- 2 million

------------ PART B ------------

maxSizeX :: Int -> [(Loc,Loc)] -> Int
maxSizeX size locs = head $ map (\(x,y) -> x * 4000000 + y) $
                     concatMap (\r -> openSquareInRow (0,0) (size,size) r sAndR (map fst locs ++ map snd locs)) [0..size]
                  -- filter (not . inRangeOfAny) $ concatMap (\r -> map (,r) [0..size]) [0..size]
    where sAndR = map (\(sens,beac) -> (sens,calculateRange sens beac)) locs

openSquareInRow :: Loc -> Loc -> Int -> [(Loc,Int)] -> [Loc] -> [Loc]
openSquareInRow (minx,_) (maxx,_) row beacRanges known =
     map (,row)
     $ concatMap (uncurry enumFromTo)
     $ foldl' (\rRow (loc,range) -> subMaybeRange rRow (computeRangeInRow loc range row))
          [(minx,maxx)] beacRanges

partB :: Input -> OutputB
partB = maxSizeX 4000000


------------ Tests  ------------

testInput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser (openInRow' 10) testInput 26,
                  TestCase $ U.inputTest inputParser (maxSizeX 20) testInput 56000011
                ]
