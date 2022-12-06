module Days.Day05 (runDay) where
    
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
import Data.Char (isDigit)
import Data.Functor (($>))
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Input <$> (makeStacks <$> readStacks <* stackNumbers <* string "\n\n") <*> sepBy instruction (char '\n')

makeStacks :: [String] -> Vector String
makeStacks = Vec.fromList . map (dropWhile (== ' '))

readStacks :: Parser [String]
readStacks = transpose <$> many1 readLine

readLine :: Parser String
readLine = sepBy box (char ' ') <* char '\n'

stackNumbers :: Parser ()
stackNumbers = many1 (satisfy (\c -> c == ' ' || isDigit c)) $> ()

box :: Parser Char
box = (char '[' *> anyChar <* char ']') <|> (string "   " $> ' ')

instruction :: Parser Instr
instruction = Instr <$> (string "move " *> (read <$> many1 digit)) <*> (string " from " *> (subtract 1 . read <$> many1 digit)) <*> (string " to " *> (subtract 1 . read <$> many1 digit))

------------ TYPES ------------
data Input = Input (Vector String) [Instr] deriving (Show)

data Instr = Instr {number :: Int, from :: Int, to :: Int} deriving (Show)

type OutputA = String

type OutputB = String

------------ PART A ------------
moveN :: Int -> [a] -> [a] -> ([a],[a])
moveN n a b = (rest,reverse taken ++ b)
    where (taken,rest) = splitAt n a


partA :: Input -> OutputA
partA (Input start instrs) = map head $ Vec.toList $ foldl' (makeMove moveN) start instrs -- TLNGFGMFN

makeMove :: (forall a. Int -> [a] -> [a] -> ([a],[a])) -> Vector String -> Instr -> Vector String
makeMove moven v (Instr amount from to) = v Vec.// [(from,from'),(to,to')]
    where (from',to') = moven amount (v Vec.! from) (v Vec.! to)


------------ PART B ------------

moveN' :: Int -> [a] -> [a] -> ([a],[a])
moveN' n a b = (rest,taken ++ b)
    where (taken,rest) = splitAt n a

partB :: Input -> OutputB
partB (Input start instrs) = map head $ Vec.toList $ foldl' (makeMove moveN') start instrs


------------ Tests  ------------
testInput = "    [D]\n[N] [C]\n[Z] [M] [P]\n 1   2   3\n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"


tests :: Test
tests = TestList ["partA test: " ~: TestCase (inputTest inputParser partA testInput "CMZ"),
                  "partB test: " ~: TestCase (inputTest inputParser partB testInput "MCD")]
