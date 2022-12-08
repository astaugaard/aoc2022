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
import Debug.Trace
{- ORMOLU_ENABLE -}



------------ PARSER ------------
inputParser :: Parser Input  -- yeah it would have been way easier to just use recursion in the parser to parse the thing into a tree
inputParser = many1 instr

instr :: Parser Instr
instr =
  (string "$ cd ..\n" $> GoUp) <|>
  (string "$ cd " *> (CD <$> many1 (satisfy (/= '\n'))) <* char '\n') <|>
  (Ls <$>
   (string "$ ls\n" *>
    many'
      (((\s n -> Left (n,(read s))) <$> (many1 digit <* char ' ') <*> (many1 (satisfy (/='\n')) <* char '\n')) <|>
       (string "dir " *> (Right <$> (many1 (satisfy (/= '\n')))) <* char '\n'))))

------------ TYPES ------------
type Input = [Instr] 

data Instr = GoUp | CD String | Ls [Either (String,Int) String] deriving Show

type OutputA = Int

type OutputB = Int

data DirectoryTree = DirDT String [DirectoryTree] | FileDT String Int

makeBaseFunctor ''DirectoryTree

type DirTree2 = Map Path Loc

data Loc = File Int | Dir [String] deriving Show

type Path = [String]

------------ PART A ------------
partA :: Input -> OutputA
partA i = snd $ hylo collapse buildTree $ (["/"],) $ snd $ execState (mapM executeInstr i) ([],Map.singleton ["/"] (Dir []))

buildTree :: (Path,DirTree2) -> DirectoryTreeF (Path,DirTree2)
buildTree (p,t) = case t Map.! p of
                    File size -> FileDTF (head p) size
                    Dir sub -> DirDTF (head p) $ map (\n -> (n:p,t)) sub

collapse :: DirectoryTreeF (Int,Int) -> (Int,Int)
collapse (FileDTF _ s) = (s,0)
collapse (DirDTF _ subs) = (size, if size <= 100000 then acc + size else acc)
    where size = sum $ map fst subs
          acc = sum $ map snd subs

executeInstr :: Instr -> State (Path,DirTree2) ()
executeInstr GoUp = modify (first tail)
executeInstr (CD n) = modify (first (n:))
executeInstr (Ls d) = do p <- gets fst
                         modify $ second $ (\a -> foldl' (\m (n,s) -> Map.insert (n:p) (File s) m) (Map.insert p (Dir names) a) files)
    where files :: [(String,Int)]
          (files,subDirs) = partitionEithers d
          names = map fst files ++ subDirs

------------ PART B ------------
partB :: Input -> OutputB
partB i = fromJust $ fst $ cata minSized dirTree
    where sizeUsed = fst $ cata collapse dirTree
          minSize = 30000000 - 70000000 + sizeUsed
          dirTree :: DirectoryTree
          dirTree = ana buildTree $ (["/"],) $ snd $ execState (mapM executeInstr i) ([],Map.singleton ["/"] (Dir []))
          -- could have represented this better (in the type)
          minSized :: DirectoryTreeF (Maybe Int,Int) -> (Maybe Int,Int)
          minSized (FileDTF _ s) = (Nothing,s)
          minSized (DirDTF _ subs) = if not $ null foundDirs then (Just $ minimum foundDirs, totalSize) else if totalSize > minSize then (Just totalSize,totalSize) else (Nothing,totalSize)
            where foundDirs = catMaybes $ map fst subs
                  totalSize = sum $ map snd subs

------------ Tests  ------------


t = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"


tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA t 95437, TestCase $ U.inputTest inputParser partB t 24933642]


runDay :: R.Day
runDay = R.runDay inputParser partA partB tests
