import Data.IORef
import Control.Concurrent

import Eval
import VM
import Constants

main :: IO ()
main = do
  globals <- newIORef =<< mkGlobals
  mapM_ (run globals) . lines =<< readFile "prelude.txt"
  mapM_ (run globals) . lines =<< readFile "galaxy.txt"
  forkIO uiThread
  interaction globals galaxyOpNum
