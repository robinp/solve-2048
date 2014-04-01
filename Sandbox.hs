module Sandbox where

import Main hiding (main)

import Control.Monad
import Data.Maybe
import System.Random
import System.Environment

t1 = Table $
  [[p 1, p 2],
   [e,   p 1]]

initial_table = Table $
  [[e, e,   e, e],
   [e, p 1, e, e],
   [e, e,   e, e],
   [e, e,   e, p 2]]

step :: WDecideConfig -> Table -> IO (Maybe (Dir, Table))
step dc t = do
  res <- decide dc t
  case res of
    Nothing        -> return Nothing
    Just (dir, t') -> do
      let next_tables = nexts t'
      i <- randomRIO (0, length next_tables - 1)
      return $ Just (dir, next_tables !! i)

loopSteps should_print dc t0 = go t0
  where
  go t = do
    when should_print $ print t
    res <- step dc t
    case res of 
      Nothing ->
        let max = (2^) . maximum . catMaybes . concat . unTable $ t
        in do
          when should_print $ putStrLn ("Game Over at " ++ show max)
          return max
      Just (d, t') -> do
        when should_print $ print d
        go t'

reference_config = defaultConfig
  { dc_heur = toWeighted heur3
  , dc_depth = 3
  }

fast_config = reference_config
  { dc_cut = niceCut 4 }

testConfig base_config = base_config
  { dc_heur = toWeighted heur4
  --, dc_cut = niceCut 4 , dc_depth = 3
  }

main = do
  putStrLn "Sandbox starts"
  args <- getArgs
  let fast = length args > 0
      (chatty, reps, base) = 
        if fast then (False, 1, fast_config)
        else (True, 1, reference_config)
      test = testConfig base
  putStrLn $ "Fast: " ++ show fast
  putStrLn "-----NEXT-----"
  next <- fmap sum . replicateM reps $ loopSteps chatty test initial_table
  putStrLn "-----BASE-----"
  base <- fmap sum . replicateM reps $ loopSteps chatty base initial_table
  putStrLn $ show base ++ " --> " ++ show next ++ " " ++  
               show (fromIntegral next / fromIntegral base) ++ "x improvement"
