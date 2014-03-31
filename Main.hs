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
      Nothing -> [p 1, p 2]
      _       -> []

-- | Performs random tile generation + shift, yields only if the shift makes a
--   difference.
validNexts :: Table -> [Table]
validNexts t = do
  nt <- nexts t
  shifted <- map (shift nt) directions
  guard (shifted /= nt)
  return shifted

-- * Heuristics

currentTime :: IO DiffTime
currentTime = fmap utctDayTime getCurrentTime

evalDepth 
  :: DiffTime -- ^ Absolute deadline
  -> Int  -- ^ Depth remaining
  -> Table
  -> (Table -> a)  -- ^ Heuristic
  -> (Table -> a)  -- ^ Heuristics for final state
  -> ([Maybe a] -> Maybe a) -- ^ Heuristic combiner
  -> IO (Maybe a)
evalDepth deadline d t f g c = go d t
  where
  go d t = do
    current_t <- currentTime
    if current_t >= deadline
    then return Nothing
    else case d of
      0 -> return . Just . f $ t
      _ -> let nts = validNexts t
           in if null nts
              then return . Just . g $ t
              else fmap c . sequence . map (go (d - 1)) $ nts

withDeadline
  :: (a -> b) 
  -> DiffTime  -- ^ Absolute deadline
  -> a -> IO (Maybe b)
withDeadline f deadline a = do
  current_t <- currentTime
  return $ if current_t < deadline
    then Just (f a)
    else Nothing

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

heur1 :: Table -> (Int, Double)
heur1 t = (1, feat_sum t - 0.75 * feat_extra_powers t)

targetPower = 11

final :: Table -> (Int, Double)
final t = (1, if t `containsPower` targetPower then 99999 else -99999)

eval1 :: Int -> Table -> IO (Maybe (Int, Double)) 
eval1 d t = do
  current_t <- currentTime
  let deadline = current_t + secondsToDiffTime 1
  evalDepth deadline d t heur1 final weighted

decide :: Table -> IO (Dir, Table)
decide t = do
  let start_tables = filter ((/= t) . snd) . 
                       map (\d -> (d, shift t d)) $ directions
  dir_scores <- mapM (\(d,s) -> ((\score -> ((d,s), score)) . maybe 0 snd) `fmap` eval1 2 s)
                  start_tables
  return . fst . L.maximumBy (comparing snd) $ dir_scores

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
  (best_dir, _) <- decide table
  putStrLn $ case best_dir of
    L -> "LEFT"
    R -> "RIGHT"
    U -> "UP"
    D -> "DOWN"
