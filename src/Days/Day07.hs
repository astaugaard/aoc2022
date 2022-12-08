{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Days.Day07 (runDay) where
    
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
import Test.HUnit hiding (Path, State)
import Data.Functor
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Control.Applicative
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import qualified Data.Foldable as F
{- ORMOLU_ENABLE -}

-- version without any recursion in commit before this one

data DirectoryTree = Dir (Map String DirectoryTree) | File Int deriving Show

makeBaseFunctor ''DirectoryTree

------------ PARSER ------------
collapseTrees :: [Map String DirectoryTree] -> DirectoryTree
collapseTrees ds = ana go $ Left ds
  where
    go ::
         Either ([Map String DirectoryTree]) Int
      -> DirectoryTreeF (Either ([Map String DirectoryTree]) Int)
    go (Right size) = FileF size
    go (Left trees) = DirF $ Map.unionsWith merge $ fmap (fmap conv) trees
      where
        conv (Dir t) = Left $ pure t
        conv (File size) = Right size
        merge (Left a) (Left b) = Left $ a ++ b
        merge (Right a) (Right _) = (Right a)
        merge a _ = error "a" -- should not happen

inputParser :: Parser Input
inputParser =
  collapseTrees . map Map.fromList <$> (string "$ cd /\n" *> many1 subTree) -- yeah it would have been way easier to just use recursion in the parser to parse the thing into a tree

notNewline :: Parser Char
notNewline = satisfy (/= '\n')

subTree :: Parser [(String, DirectoryTree)]
subTree =
  (string "$ ls\n" *> many subThings) <|>
  (\n s -> pure (n, (collapseTrees $ map Map.fromList s))) <$>
  (string "$ cd " *>
   (many1 notNewline >>= \i ->
      if i == ".."
        then fail "can't be up up"
        else pure i) <*
   char '\n') <*>
  (many' subTree <* option () (string "$ cd ..\n" $> ()))

subThings :: Parser (String, DirectoryTree)
subThings =
  (\s n -> (n, File (read s))) <$> many1 digit <*>
  (char ' ' *> many1 notNewline <* char '\n') <|>
  (\n -> (n, Dir (Map.empty))) <$>
  (string "dir " *> many1 notNewline <* char '\n')

------------ TYPES ------------
type Input = DirectoryTree

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = withSize collapse

withSize :: (Int -> DirectoryTreeF a -> a) -> DirectoryTree -> a
withSize f t = gcata distrib (\a -> f (sum (fmap fst a)) $ fmap snd a) t
  where
    distrib :: forall b. DirectoryTreeF (Int, b) -> (Int, DirectoryTreeF b)
    distrib (FileF i) = (i, FileF i)
    distrib (DirF sub) = (total, DirF $ fmap snd sub)
      where
        total = sum $ fmap fst $ sub

collapse :: Int -> DirectoryTreeF Int -> Int
collapse _ (FileF _) = 0
collapse size (DirF subs) =
  if size <= 100000
    then acc + size
    else acc
  where
    acc = sum subs

------------ PART B ------------
partB :: Input -> OutputB
partB i = minimum $ withSize validRemovals i
  where
    sizeUsed = withSize (\size _ -> size) i
    minSize = 30000000 - 70000000 + sizeUsed
    validRemovals :: Int -> DirectoryTreeF [Int] -> [Int]
    validRemovals size (DirF subs) =
      case valids of
        [] ->
          if size > minSize
            then [size]
            else []
        (_:_) -> valids
      where
        valids = concat $ F.toList subs
    validRemovals _ _ = []
------------ Tests  ------------


t = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"


tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA t 95437, TestCase $ U.inputTest inputParser partB t 24933642]


runDay :: R.Day
runDay = R.runDay inputParser partA partB tests
