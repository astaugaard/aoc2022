module Days.Day12 (runDay) where

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
import Data.Functor.Foldable
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Char (ord)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy line (char '\n')

line :: Parser [Char]
line = many1 (satisfy (/= '\n'))

------------ TYPES ------------

data Coord = Coord Int Int deriving (Ord,Eq,Show)

(!?) :: (Maybe (Vector a)) -> Int -> Maybe a
v !? a = v >>= (Vec.!? a)

bfs :: (Int -> Int -> Bool) -> Coord -> Vector (Vector Int) -> [Map Coord [Coord]]
bfs pred start g = ana go (Map.fromList [(start,[])],Map.singleton start ())
        -- Map Coord () is isomorphic to Set Coord but it is easier to do difference on it
    where go :: (Map Coord [Coord], Map Coord ()) -> ListF (Map Coord [Coord]) (Map Coord [Coord], Map Coord ())
          go (visitedLast,visited) | Map.null minusAlreadyVisited = Nil
                                   | otherwise = Cons minusAlreadyVisited (minusAlreadyVisited,(minusAlreadyVisited $> ()) `Map.union` visited)
            where canVisit = Map.fromList $ concatMap canVisitFrom $ Map.toList visitedLast
                  canVisitFrom :: (Coord,[Coord]) -> [(Coord,[Coord])]
                  canVisitFrom (from@(Coord x y),toGetTo) = map (\c@(Coord x' y') -> (c,from:toGetTo))
                                $ filter (\(Coord x' y') -> case g Vec.!? y' !? x' of
                                    Just other -> case (g Vec.!? y !? x) of
                                        Just current ->  other `pred` current 
                                        Nothing -> False
                                    Nothing -> False)
                                $ [Coord x (y-1),Coord x (y+1), Coord (x-1) y, Coord (x+1) y]
                                        -- [Coord x' y' | x' <- [x-1..x+1]
                                        --        , y' <- [y-1..y+1]
                                        --        , not (x == x' && y == y')]
                  minusAlreadyVisited = canVisit `Map.difference` visited
                  
            -- get things that we can visit this round
            -- remove every thing that we have already visited
            -- take the things that we are visiting
            --    if it is empty return NIL
            --    if it has values return a what we visited and how we got to it
            --      and have the continuation of the parse be the coords that we just visited and how to get to them
            --


type Input = [[Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA i = (+1) . length $ Data.List.takeWhile (not . Map.member end) atDistances
    where [start] = map fst $ filter ((=='S') . snd) withLocs
          [end] = map fst $ filter ((=='E') . snd) withLocs
          withLocs = concat $ zipWith (\r v -> zipWith (\c v' -> (Coord c r, v')) [0..] v) [0..] i
          heighMap = Vec.fromList $ map (Vec.fromList . map (\v -> if v == 'S' then 0 else if v == 'E' then 25 else ord v - ord 'a')) i
          atDistances :: [Map Coord [Coord]]
          atDistances = bfs (\other current -> other <= current + 1) start heighMap
          -- heightMap =

------------ PART B ------------
partB :: Input -> OutputB
partB i = (+1) . length $ Data.List.takeWhile (\a -> not $ any ((==0) . atLoc) $ map fst $ Map.toList a) $ atDistances
    where [end] = map fst $ filter ((=='E') . snd) withLocs
          withLocs = concat $ zipWith (\r v -> zipWith (\c v' -> (Coord c r, v')) [0..] v) [0..] i
          heighMap = Vec.fromList $ map (Vec.fromList . map (\v -> if v == 'S' then 0 else if v == 'E' then 25 else ord v - ord 'a')) i
          atDistances :: [Map Coord [Coord]]
          atDistances = bfs (\other current -> current <= other + 1) end heighMap
          atLoc (Coord x y) = heighMap Vec.! y Vec.! x

------------ Tests  ------------

testInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"


tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testInput 31
                 ,TestCase $ U.inputTest inputParser partB testInput 29]
