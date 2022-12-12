{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.List as L
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
import Test.HUnit hiding (State)
import Util.Util (inputTest)
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))
import Data.Sequence
import Control.Monad.State
import Debug.Trace
import qualified Data.List.Split as S
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy instr (char '\n')

instr :: Parser Instr
instr = string "noop" $> NOp
    <|> AddX <$> (string "addx " *> ((\sign s -> sign * read s) <$> option 1 (char '-' $> -1) <*> many1 digit))

------------ TYPES ------------
type Input = [Instr]

data Instr = AddX Int | NOp deriving Show

type OutputA = Int

type OutputB = RawString

newtype RawString = RawString String deriving Eq

instance Show RawString where
    show (RawString s) = s

-- data InterpreterState = InterpreterState {toRun :: Seq Instr, currentValue :: Int}
-- data InterpreterState = InterpreterState {cycleCount :: Int, currentValue :: Int}

------------ PART A ------------

-- executeCycle :: Instr -> State InterpreterState Int
-- executeCycle i = do modify updateValue
--                     gets currentValue
--     where updateValue :: InterpreterState -> InterpreterState
--           updateValue (InterpreterState seq c) = case viewl seq of
--             EmptyL -> InterpreterState (Data.Sequence.singleton i) c
--             (NOp :< as) -> InterpreterState (as |> i) c
--             (AddX a :< as) -> InterpreterState (as |> i) (c + a)

makeStates :: [Instr] -> [Int]
makeStates i = states
   where states = L.unfoldr go (i,0,1)
         go (i@(AddX v:is),0,c) = Just (c,(i,1,c))
         go (AddX v:is,1,c) = Just (c,(is,0,c+v))
         go (NOp:is,_,c) = Just (c,(is,0,c))
         go ([],_,_) = Nothing
partA :: Input -> OutputA
partA i = sum $ map (uncurry (*)) $ L.filter ((==0) . (`mod` 40) . subtract 20 . fst) $ L.zip [1..] states
    where states = makeStates i
    -- sum $ map (uncurry (*)) $ L.filter ((==0) . (`mod` 40) . subtract 20 . fst) $ (\a -> trace (show a) a) $ L.zip ([1..] :: [Int]) $ evalState (forM i executeCycle) (InterpreterState (singleton NOp) 1)


------------ PART B ------------
partB :: Input -> OutputB
partB i = RawString $ L.intercalate "\n" $ S.chunksOf 40 $ L.zipWith (\l v -> if abs (((l-1) `mod` 40) - v) <= 1 then '#' else '.') [1..] states
    where states = makeStates i


------------ Tests  ------------
testI = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"

testO = RawString "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######....."

tests :: Test
tests = TestList [TestCase $ inputTest inputParser partA testI 13140,
                  TestCase $ inputTest inputParser partB testI testO]
