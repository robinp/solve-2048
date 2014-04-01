{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad
import Data.Foldable (foldl')
import qualified Data.List as L
import Data.Time.Clock
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace
import System.Random

-- * Representation

type Pos = (Int,Int)
type Power = Int  -- ^ Power of two, from 1 (= tile 2)

newtype Table = Table { unTable :: [[Maybe Power]] }
  deriving (Eq, Ord)

instance Show Table where
  show (Table rows) = L.intercalate "\n" (map showRow rows) ++ "\n"
showRow = concat . map (("\t" ++) . maybe "_" show)

containsPower (Table rows) p =
  any (any (== Just p)) rows 

data Dir = L | R | U | D
  deriving (Show)

directions = [L, R, U, D]

-- * Shifting

p = Just
e = Nothing

shiftLeft :: [Maybe Int] -> [Maybe Int]
shiftLeft row =
  let spaceless = catMaybes row
  in shiftL (length row) spaceless
  where
  shiftL :: Int -> [Int] -> [Maybe Int]
  shiftL remain nums = case nums of
    []                -> replicate remain Nothing
    (x:y:rs) | x == y -> Just (x+1):shiftL (remain - 1) rs
    (x:rs)            -> Just x:shiftL (remain - 1) rs

shiftRight = reverse . shiftLeft . reverse

shift :: Table -> Dir -> Table
shift t dir = Table $ unTable t `appliedOn` case dir of
  L -> map shiftLeft
  R -> map shiftRight
  U -> withTransposed (map shiftLeft)
  D -> withTransposed (map shiftRight) 
  where
  withTransposed f = L.transpose . f . L.transpose
  appliedOn = flip ($)

-- * Game tree

substitute :: (a -> [a]) -> [a] -> [[a]]
substitute _ [] = []
substitute f (a:as) = sub [] a as []
  where
  sub pre a [] res =
    map (\x -> reverse (x:pre)) (f a) ++ res
  sub pre a (r:rs) res = 
    sub (a:pre) r rs res'
    where
    res' = map (\x -> reverse pre ++ [x,r] ++ rs) (f a) ++ res

as `substWith` f = substitute f as

nexts :: Table -> [Table]
nexts (Table rows) =
  map Table . substWith rows $ \row ->
    substWith row $ \elem -> case elem of
      Nothing -> [p 1] -- , p 2]  -- p 2 has lower change, let's not care
      _       -> []

type Cut = forall a . [a] -> IO [a]

-- | Performs random tile generation + shift, yields only if the shift makes a
--   difference.
validNexts :: Cut -> Table -> IO [Table]
validNexts c t = do
  nts <- c . nexts $ t :: IO [Table]
  return $ do
    nt <- nts
    shifted <- map (shift nt) directions
    guard (shifted /= nt)
    return shifted

-- * Heuristics

currentTime :: IO DiffTime
currentTime = fmap utctDayTime getCurrentTime

evalDepth 
  :: DiffTime -- ^ Absolute deadline
  -> DecideConfig a
  -> Table
  -> ([Maybe a] -> Maybe a) -- ^ Heuristic combiner
  -> IO (Maybe a)
evalDepth deadline dc t c = go (dc_depth dc) t
  where
  go d t = do
    current_t <- currentTime
    if current_t >= deadline
    then return Nothing
    else case d of
      0 -> return . Just . f $ t
      _ -> do
        nts <- validNexts cut t
        if null nts
           then return . Just . f $ t
           else fmap c . sequence . map (go (d - 1)) $ nts
    where
    cut = dc_cut dc
    f = dc_heur dc

weighted :: [Maybe (Int, Double)] -> Maybe (Int, Double)
weighted ms = case catMaybes ms of
  [] -> Nothing
  xs -> let sum_w = sum . map fst $ xs
            total = sum . map (\(w,x) -> fromIntegral w * x) $ xs
        in Just (sum_w, total / fromIntegral sum_w)

feat_extra_powers :: Table -> Double
feat_extra_powers (Table rows) =
  let power_counts = map (\(x:xs) -> (x, 1 + length xs)) . L.group . L.sort .
                       catMaybes . concat $ rows 
  in fromIntegral . sum . 
       map (\(power, cnt) -> (cnt - 2)*(2^power)) . 
       filter ((> 2) . snd) $ power_counts

feat_sum :: Table -> Double
feat_sum (Table rows) =
  fromIntegral . sum . map (2^) . catMaybes . concat $ rows

feat_merge :: Table -> Double
feat_merge (Table rows) =
  let row_score = sum . map rowMerge $ rows
      col_score = sum . map rowMerge . L.transpose $ rows
  in row_score + col_score
  where
  rowMerge = fst . foldl' (\(s,l) r -> (s + pairFun l r,r)) (0, Nothing)
  pairFun (Just p) (Just r) =
    if abs (p - r) == 1 then 2 ^ (min p r)
    else 0  -- or penalize? 
  pairFun _ _ = 0

feat_ordered :: Table -> Double
feat_ordered (Table rows) = go False 999 rows
  where
  go _ _ [] = 0
  go should_rev smallest (r:rs) =
    let (score, sm') = rowOrder smallest (if should_rev then reverse r else r)
    in score + go (not should_rev) sm' rs
  rowOrder smallest xs = 
    -- count empty space as small, so score is penalizing it
    let powers = map (maybe 0 id) xs
    in foldl' (\(score, sm) p ->
                if p <= sm then (score + 2^p, p) else (score - 2^p, sm))
              (0, smallest) $ powers

feat_ordered_rel :: Table -> Double
feat_ordered_rel (Table rows) = go False 999 rows
  where
  go _ _ [] = 0
  go should_rev prev (r:rs) =
    let (score, pre') = rowOrder prev (if should_rev then reverse r else r)
    in score + go (not should_rev) pre' rs
  rowOrder previous xs = 
    let powers = catMaybes xs
    in foldl' (\(score, prev) p ->
                if p <= prev then (score + 2^p, p) else (score, prev))
              (0, previous) $ powers

feat_free :: Table -> Double
feat_free (Table rows) = 
  let empties = length . filter (== Nothing) . concat $ rows
      max_value = case catMaybes . concat $ rows of
        [] -> 0
        xs -> 2 ^ maximum xs
  in fromIntegral (empties * max_value) / 16.0  -- tile_count

feat_sticky :: Table -> Double
feat_sticky (Table rows) =
  sum . map (2^) . catMaybes . head $ rows

type Heur = Table -> (Int, Double)

toWeighted f = \t -> (1, f t)

niceCut :: Int -> Cut
niceCut n xs = do
  rnds <- replicateM (length xs) (randomRIO (0, 1000)) :: IO [Int]
  let ys = map snd . L.sortBy (comparing fst) . zip rnds $ xs
  return $ take n ys

heur1 t = 1.0  * feat_sum t 
        - 0.75 * feat_extra_powers t

heur2 t = 1.0  * feat_sum t 
        - 0.75 * feat_extra_powers t
        + 1.0  * feat_free t

heur3 t = 0 --1.0  * feat_sum t 
        + 1.0  * feat_free t

heur4 t = 1.0  * feat_free t
        -- + 0.25 * feat_ordered_rel t
        + 0.1  * feat_sticky t

heur5 t = 1.0  * feat_free t
        + 1.0  * feat_ordered t

evalW :: WDecideConfig -> Table -> IO (Maybe (Int, Double)) 
evalW dc t = do
  current_t <- currentTime
  let deadline = current_t + picosecondsToDiffTime (200 * 1000 * 1000 * 1000)
  evalDepth deadline dc t weighted

data DecideConfig a = DecideConfig
  { dc_heur :: Table -> a
  , dc_depth :: Int
  , dc_cut :: Cut
  }

type WDecideConfig = DecideConfig (Int, Double)

defaultConfig = DecideConfig (toWeighted heur1) 2 (return . id)
bestConfig = defaultConfig 
  { dc_heur = toWeighted heur3
  , dc_depth = 3
  }

decide :: WDecideConfig -> Table -> IO (Maybe (Dir, Table))
decide dc t = do
  let start_tables = filter ((/= t) . snd) . 
                       map (\d -> (d, shift t d)) $ directions
  dir_scores <- mapM (\(d,s) -> ((\score -> ((d,s), score)) . maybe 0 snd) 
                       `fmap` evalW dc s)
                  start_tables
  if null dir_scores
    then return Nothing
    else return . Just . fst . L.maximumBy (comparing snd) $ dir_scores


-- * Parse and main

numConv :: Int -> Maybe Power
numConv 0 = Nothing
numConv x = Just $ case x of
  1 -> error "unexpected 1"
  other -> log2 other
  where
  log2 2 = 1
  log2 x = 1 + log2 (x `div` 2) 

main = do
  input <- getContents
  let table = Table . map (map (numConv . read) . words) . lines $ input
  Just (best_dir, _) <- decide bestConfig table
  putStrLn $ case best_dir of
    L -> "LEFT"
    R -> "RIGHT"
    U -> "UP"
    D -> "DOWN"
