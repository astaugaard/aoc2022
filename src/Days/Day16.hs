module Days.Day16 (runDay) where

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
import Data.Attoparsec.Text hiding (takeWhile)
import Data.Void
import Test.HUnit
import Data.MemoTrie
import Data.Bifunctor (second)
import Debug.Trace
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> sepBy valve (char '\n')

valve :: Parser (String, (Int, [String]))
valve = (,) <$>
    (string "Valve " *> many1 letter) <*>
    ((,) <$> (string " has flow rate=" *> (read <$> many1 digit)) <*>
    (string "; tunnel" *> option ' ' (char 's') *> " lead" *> option ' ' (char 's') *> " to valve" *> (string "s " <|> string " ") *> sepBy1 (many1 letter) (string ", ")))

------------ TYPES ------------
type Input = Map String (Int,[String])

type OutputA = Int

type OutputB = Void

data Action = Goto String | Open

------------ PART A ------------

possibleActions :: Map String (Int,[String]) -> [(String,Bool)] -> String -> [Action]
possibleActions locmap openMap loc = (case lookup loc openMap of
                                        Nothing -> id
                                        Just False -> (Open :)
                                        Just True -> id)
                                     $ map Goto
                                     $ snd (locmap Map.! loc)

totalValuePointTime :: Map String (Int,[String]) -> Int -> String -> Int
totalValuePointTime locmap remTime loc = memoFix (\f (t,l,open) ->
                                                    if t > 0 then
                                                        maximum $ map (runAction f open t l) $ possibleActions locmap open l
                                                    else 0)
                                                 (remTime,loc,map (\(s,_) -> (s,False)) $ Map.toList locmap)
    where runAction :: ((Int,String,[(String,Bool)]) -> Int) -> [(String,Bool)] -> Int -> String -> Action -> Int
          runAction f open t _ (Goto str) = f (t - 1,str,open)
          runAction f open t l Open = fst (locmap Map.! l) * (t - 1) +
                                      f (t - 1,l,pre ++ (second (const True) h : post))
            where (pre,h:post)= span ((l /=) . fst) open

partA :: Input -> OutputA
partA i = totalValuePointTime (trace (show i) i) 30 "AA"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"


------------ Tests  ------------

testInput = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"

tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testInput 1651]
