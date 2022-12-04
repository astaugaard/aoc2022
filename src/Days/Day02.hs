module Days.Day02 (runDay) where

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
import Data.Functor (($>))
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 game $ char '\n'

game :: Parser (Move, Move)
game = (,) <$> ((char 'A' $> Rock) <|> (char 'B' $> Paper) <|> char 'C' $> Scissors) <*> (char ' ' *> ((char 'X' $> Rock) <|> (char 'Y' $> Paper) <|> (char 'Z' $> Scissors)))

------------ TYPES ------------
data Move = Rock | Paper | Scissors deriving (Show, Eq)

type Input = [(Move, Move)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

value :: Move -> Int
value Rock = 1
value Paper = 2
value Scissors = 3

result :: Move -> Move -> Int -- returns the result for second move (the points)
result mo mo'
  | diff == 0 = 3 -- yep this is a mess I don't know if this was worth it just to avoid a case statement...
  | diff == 1 = 0
  | diff == 2 = 6
  | diff == (-1) = 6
  | diff == (-2) = 0
  | otherwise = error "error"
  where
    diff = value mo - value mo'

partA :: Input -> OutputA
partA = sum . map (\(a, b) -> result a b + value b)

------------ PART B ------------

toWin :: Move -> Move
toWin Rock = Paper
toWin Paper = Scissors
toWin Scissors = Rock

toLose :: Move -> Move
toLose Rock = Scissors
toLose Paper = Rock
toLose Scissors = Paper

toTie :: Move -> Move
toTie = id

partB :: Input -> OutputB
partB =
  sum
    . map
      ( \(a, b) ->
          ( case b of
              Rock -> 0
              Paper -> 3
              Scissors -> 6
          )
            + value
              ( case b of
                  Rock -> toLose a
                  Paper -> toTie a
                  Scissors -> toWin a
              )
      )

------------ Tests  ------------
tests :: Test
tests = TestList []
