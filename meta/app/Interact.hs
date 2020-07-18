import Data.IORef
import Control.Concurrent
import System.Environment (getArgs)

import Eval
import VM
import Constants

main :: IO ()
main = do
  globals <- newIORef =<< mkGlobals
  mapM_ (run globals) . lines =<< readFile "prelude.txt"
  mapM_ (run globals) . lines =<< readFile "galaxy.txt"
  forkIO uiThread
  args <- getArgs
  if null args
  then interaction globals galaxyOpNum []
  else do
      moves <- (map read . lines) `fmap` (readFile $ head args)
      interaction globals galaxyOpNum moves
