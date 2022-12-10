-- {-# LANGUAGE DerivingFunctor #-}
{-# OPTIONS_GHC -Wall #-}
module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (Array)
import qualified Data.Array as Ar
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (I)
import Data.Void
import Test.HUnit
import Data.Char
import Control.Comonad
import Data.Foldable (toList)
-- import Control.Comonad.Store.Pointer
import Data.Function
import Debug.Trace
import Control.Comonad.Sheet
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB tests

-- instance Comonad IGrid where
--     extract (Grid g i) = g Ar.! i
--     duplicate (Grid g (x,y)) = Grid (Vec.imap (\y' s -> Vec.imap (\x' _ -> Grid g (x',y')) s) g) (x,y)

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 (map digitToInt <$> many1 digit) (char '\n')

-- makeGrid :: [[a]] -> Grid a
-- makeGrid v = $ pointer (Ar.listArray ((0,0),(length (head v),length v)) (concat v)) (0,0)

------------ TYPES ------------
type Input = [[Int]]


type Grid a = Sheet2 (Maybe a)

-- data IGrid a = Grid {getGrid :: Array (Int,Int) a, loc :: (Int,Int)} deriving (Functor, Foldable,Show)
-- instance Comonad IGrid where
--   extract (Grid a i) = a Ar.! i
--   duplicate (Grid a (cx,cy)) = Grid (Ar.array ((0,0),(bx,by))
--     $ [((x,y),(Grid a (x,y)))
--         | 
--         x <- [0..bx],
--         y <- [0..by]
--       ]) (cx,cy)
--     where (_,(bx,by)) = Ar.bounds a

-- instance Applicative IGrid where
--     (<*>) fs as = Grid (Vec.zipWith (\fss ass -> Vec.zipWith ($) fss ass) (getGrid fs) $ getGrid as) $ loc fs
-- instance ComonadApply IGrid where

-- zapGrid :: Grid (a -> b) -> Grid a -> Grid b
-- zapGrid fs as = if Ar.bounds fa /= Ar.bounds aa || i /= ia then error "not implemented" else pointer (Ar.array (Ar.bounds aa) $ map (\i -> (i, trace "hello there" $ (fa Ar.! i) (aa Ar.! i))) $ Ar.indices fa) i
--     where ~(fa,i) = runPointer fs
--           ~(aa,ia) = runPointer as

-- gridFix :: Grid (Grid a -> a) -> Grid a
-- gridFix funcs = trace "test" fix $ \a -> trace "meow2" $ funcs `zapGrid` (trace "meow3" $ duplicate a)

-- currently doesn't work for comonads if they are focused on different points

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- at :: Int -> Int -> Grid a -> Maybe a
-- at x y g' = if Ar.inRange (Ar.bounds g) (fst i + x, snd i + y) then Just $ g Ar.! (fst i + x, snd i + y) else Nothing
--     where ~(g, i) = runPointer g'

-- computeVis :: Int -> Int -> Int
-- computeVis otherVis height = _
-- computeVis otherVis height = _

computeViss :: Grid (Int,Int,Int,Int) -> Grid Int -> (forall a. Grid a -> Maybe a) -> ((Int,Int,Int,Int) -> Int) -> Int
computeViss visG hG loc dir = fromMaybe (-1) (max <$> loc hG <*> (dir <$> loc visG))

visGrid :: Grid Int -> Grid Bool
visGrid ig = vissible <$> ig <@> viss
    where toAreaFunction :: Grid Int -> (Grid (Int,Int,Int,Int) -> Maybe (Int,Int,Int,Int))
          toAreaFunction g vis = Just -- trace (show (snd (runPointer vis)))))))
            ( locDir (cell above) (\(~(a,_,_,_)) -> a)
            , locDir (cell left) (\(~(_,b,_,_)) -> b)
            , locDir (cell right)  (\(~(_,_,c,_)) -> c)
            , locDir (cell below) (\(~(_,_,_,d)) -> d)
            )
            where locDir :: (forall a. Grid a -> Maybe a) -> ((Int,Int,Int,Int) -> Int) -> Int
                  locDir = computeViss vis g
          viss :: Grid (Int,Int,Int,Int)
          viss = evaluate $ extend toAreaFunction ig
          vissible :: Maybe Int -> Maybe (Int,Int,Int,Int) -> Maybe Bool
          vissible (Just h) (Just (a,b,c,d)) = Just $ any (h>) [a,b,c,d]
          vissible _ _ = Nothing

partA :: Input -> OutputA
partA i = length $ filter fromJust $ concat $ Control.Comonad.Sheet.take (belowBy (gh - 1) Control.Comonad.Sheet.& rightBy (gw - 1)) viss
    where
          gh = length i
          gw = length $ head i
          ig = sheet Nothing $ map (map Just) i
          viss = visGrid ig



------------ PART B ------------
partB :: Input -> OutputB
partB i = maximum s
    where scenicScore :: Grid Int
          scenicScore = extend computeDirScore ig
          computeDirScore :: Grid Int -> Maybe Int
          computeDirScore h = Just
                (dir (go above) *
                 dir (go right) *
                 dir (go below) *
                 dir (go left))
            where
              dir :: (Grid Int -> Grid Int) -> Int
              dir l = case extract h of
                        Just h' -> let (b,a) = span (<h') $ unfoldr (\n -> (,) <$> extract n <*> pure (l n)) $ l h
                                   in if null a then length b else length b + 1
                        Nothing -> 0
          -- viss = visGrid ig
          ig :: Grid Int
          ig = sheet Nothing $ map (map Just) i
          gh = length i
          gw = length $ head i
          s :: [Int]
          s = map fromJust
            $ concat
            $ Control.Comonad.Sheet.take (belowBy (gh - 1) Control.Comonad.Sheet.& rightBy (gw - 1))
              scenicScore
------------ Tests  ------------

testInput = "30373\n25512\n65332\n33549\n35390"

tests :: Test
tests = TestList [TestCase $ U.inputTest inputParser partA testInput 21,
                  TestCase $ U.inputTest inputParser partB testInput 8]
