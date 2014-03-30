module Main where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- * Representation

type Pos = (Int,Int)
type Power = Int  -- ^ Power of two, from 1 (= tile 2)

newtype Table = Table { unTable :: [[Maybe Power]] }
  deriving (Eq, Ord)

instance Show Table where
  show (Table rows) = L.intercalate "\n" (map showRow rows) ++ "\n"
showRow = concat . map (("\t" ++) . maybe "_" show)

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
