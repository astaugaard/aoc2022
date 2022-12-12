{-# LANGUAGE TemplateHaskell #-}

module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (Array)
import qualified Data.Array as Array
import qualified Util.Util as U
import Lens.Micro.Platform
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (State)
import Control.Monad.State
import Data.Void
import Test.HUnit hiding (State)
import Control.Applicative (Alternative((<|>)))
{- ORMOLU_ENABLE -}

data CurrentState = CurrentState {_hloc :: (Int, Int), _tloc :: (Int, Int), _tailOn :: Set (Int, Int)}

makeLenses ''CurrentState

data CurrentState2 = CurrentState2 {_headL :: (Int, Int), _tailLocs :: [(Int, Int)], _beenOn :: Set (Int, Int)}

makeLenses ''CurrentState2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy row (char '\n')

row :: Parser (Dir, Int)
row = (,) <$> dir <*> (char ' ' *> (read <$> many1 digit))

dir :: Parser Dir
dir =
  U <$ char 'U'
    <|> Do <$ char 'D'
    <|> L <$ char 'L'
    <|> R <$ char 'R'

------------ TYPES ------------
type Input = [(Dir, Int)]

data Dir = U | Do | L | R deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------

oneStep :: Dir -> State CurrentState ()
oneStep dir = do
  case dir of
    U -> hloc . _2 += 1
    Do -> hloc . _2 -= 1
    L -> hloc . _1 -= 1
    R -> hloc . _1 += 1
  h <- use hloc
  t <- use tloc
  let t' = updateTail h t
  tloc .= t'
  updateSet

updateSet :: State CurrentState ()
updateSet = do
  l <- use tloc
  tailOn %= Set.insert l

updateTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateTail h@(xh, yh) t@(xt, yt) = if abs (xt - xh) > 1 || abs (yt - yh) > 1 then moveTail h t else t

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (xh, yh) (xt, yt) = (moveCord xh xt, moveCord yh yt)
  where
    moveCord h t
      | t > h = t - 1
      | t < h = t + 1
      | otherwise = t

partA :: Input -> OutputA
partA i = Set.size (resultState ^. tailOn)
  where
    moves = concatMap (uncurry (flip replicate)) i
    resultState = execState (forM moves oneStep) $ CurrentState (0, 0) (0, 0) Set.empty

------------ PART B ------------

oneStep' :: Dir -> State CurrentState2 ()
oneStep' dir = do
  case dir of
    U -> headL . _2 += 1
    Do -> headL . _2 -= 1
    L -> headL . _1 -= 1
    R -> headL . _1 += 1
  h <- use headL
  tail <- use tailLocs
  let tail' = advanceTails h tail
  tailLocs .= tail'
  beenOn %= Set.insert (last tail') -- this could be faster if I made it so that it doesn't have to loop over the list again by
                                    -- storing the tail elements in reverse and doing a fold instead of a unfold on the list
                                    -- or I could have also used a vector or an array to store the tail elements and just indexed
                                    -- to the end of it

advanceTails :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
advanceTails h t = unfoldr go (h,t)
    where go (h,[]) = Nothing
          go (h,x:xs) = Just (nt,(nt,xs))
            where nt = updateTail h x


partB :: Input -> OutputB
partB i = Set.size (resultState ^. beenOn)
  where
    moves = concatMap (uncurry (flip replicate)) i
    resultState = execState (forM moves oneStep') $ CurrentState2 (0, 0) (replicate 9 (0, 0)) Set.empty

------------ Tests  ------------

testI = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

testI2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testI 13, TestCase $ U.inputTest inputParser partB testI2 36]

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests
