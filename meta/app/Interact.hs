import Data.IORef

import Eval
import VM
import Constants

main :: IO ()
main = do
  globals <- newIORef =<< mkGlobals
  mapM_ (run globals) . lines =<< readFile "prelude.txt"
  mapM_ (run globals) . lines =<< readFile "galaxy.txt"
  interaction globals galaxyOpNum
