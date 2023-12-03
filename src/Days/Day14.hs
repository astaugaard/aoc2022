module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (takeWhile)
import Data.Void
import Test.HUnit
import Debug.Trace (trace)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 line (char '\n')

line :: Parser [(Int,Int)]
line = sepBy1 coord (string " -> ")

num :: Parser Int
num = read <$> many1 digit

coord :: Parser (Int,Int)
coord = (,) <$> num <*> (char ',' *> num)

------------ TYPES ------------
type Input = [[(Int,Int)]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

inputToSet :: [[(Int,Int)]] -> Set (Int,Int)
inputToSet lines = Set.fromList
                 $ do line <- lines
                      ((a1,a2),(b1,b2)) <- zip line $ tail line
                      c1 <- [min a1 b1 .. max a1 b1]
                      c2 <- [min a2 b2 .. max a2 b2]
                      return (c1,c2)

maxY :: [[(Int,Int)]] -> Int
maxY l = maximum $ map snd $ concat $ l

dropSand :: Int -> Set (Int,Int) -> (Set (Int,Int),Bool)
dropSand maxy a = (Set.insert added a, snd added > maxy)
    where ret x y = Just ((x,y),(x,y))
          added = last $
                  unfoldr (\(x,y) -> if y > maxy then Nothing
                                     else if not $ Set.member (x,y+1) a then ret x (y+1)
                                     else if not $ Set.member (x-1,y+1) a then ret (x-1) (y+1)
                                     else if not $ Set.member (x+1,y+1) a then ret (x+1) (y+1)
                                     else Nothing) (500,0)
partA :: Input -> OutputA
partA i = (subtract 1) $ length $ takeWhile (not . snd) $ iterate (dropSand (maxY i) . fst) (inputToSet i, False)

------------ PART B ------------


dropSand' :: Int -> Set (Int,Int) -> (Set (Int,Int),(Int,Int))
dropSand' maxy a = (Set.insert added a, added)
    where ret x y = Just ((x,y),(x,y))
          added = last
                $ ((500,0):)
                $ unfoldr (\(x,y) -> if y >= (maxy + 1) then Nothing
                                     else if not $ Set.member (x,y+1) a then ret x (y+1)
                                     else if not $ Set.member (x-1,y+1) a then ret (x-1) (y+1)
                                     else if not $ Set.member (x+1,y+1) a then ret (x+1) (y+1)
                                     else Nothing) (500,0)
partB :: Input -> OutputB
partB i = length $ takeWhile ((/= (500,0)) . snd) $ iterate (dropSand' (maxY i) . fst) (inputToSet i, (0,0))

------------ Tests  ------------

testInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"


tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testInput 24,
                  TestCase $ U.inputTest inputParser partB testInput 93]
