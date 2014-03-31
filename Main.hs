module Main where

import Control.Monad
import Data.Foldable (foldl')
import qualified Data.List as L
import Data.Time.Clock
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
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
      Nothing -> [Just 2, Just 4]
      _       -> []

-- * Heuristics

currentTime :: IO DiffTime
currentTime = fmap utctDayTime getCurrentTime

evalDepth 
  :: Int  -- ^ Depth remaining
  -> Table
  -> (Table -> IO (Maybe a))  -- ^ Heuristic (time-capped)
  -> (Table -> a)  -- ^ Heuristics for final state
  -> ([Maybe a] -> Maybe a) -- ^ Heuristic combiner
  -> IO (Maybe a)
evalDepth d t f g c = go d t
  where
  go 0 t = f t
  go d t = 
    let nts = nexts t  -- TODO use version having directions here
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

heur1 :: Table -> Double
heur1 t = 0.0

worst :: [Maybe Double] -> Maybe Double
worst ms = trace (show ms) $ case catMaybes ms of
  [] -> Nothing
  x:xs -> Just (foldl' min x xs)

targetPower = 11

final :: Table -> Double
final t = if t `containsPower` targetPower then 99999 else -99999

eval1 :: Int -> Table -> IO (Maybe Double) 
eval1 d t = do
  current_t <- currentTime
  let deadline = current_t + secondsToDiffTime 1
  evalDepth d t (withDeadline heur1 deadline) final worst 

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
  print table
