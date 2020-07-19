{-# LANGUAGE LambdaCase, TemplateHaskell #-}
import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception
import System.IO
import Language.Haskell.TH
import System.Process
import Common
import Protocol

runHTTP :: String -> Protocol.Request -> IO Protocol.Response
runHTTP uri req = do
  let sreq = map (\case True -> '1'; False -> '0') $ modulate $ toProto req
  request <- setRequestBodyLBS (BLU.fromString sreq) <$> parseRequest ("POST " ++ uri ++ "/aliens/send")
  response <- httpLBS request
  case show (getResponseStatusCode response) of
    "200" -> do
        let sresp = BLU.toString $ getResponseBody response
        case fromProto $ demodulate $ (== '1') <$> sresp of
          Just resp -> pure resp
          _ -> error $ "Could not parse: " ++ show (demodulate $ map (=='1') sresp)
    _ -> error $ "Server error: " ++ show response

main = do
  hPutStrLn stderr $(LitE . StringL <$> runIO (readProcess "git" ["rev-parse", "HEAD"] ""))
  [uri, skey] <- getArgs
  let key = GameId $ read skey
  let
    go (RespGame Finished _ _) = pure ()
    go (RespGame Started info (Just state)) = do
      let myShips = map fst $ filter (\(s, _) -> shipTeam s == myTeam info) $ gameShips state
      runHTTP uri (ReqAct key [Boost (shipId s) (quadrant (shipPos s)) | s <- myShips]) >>= go
    go resp = error $ show resp
  runHTTP uri (ReqJoin key [103652820,192496425430]) >>= \case
    RespGame NotStarted info _ -> do
      let m = maxTotal $ maxStats info
      runHTTP uri (ReqStart key (Just $ Stats (m - 2) 0 0 1)) >>= go
    where
      quadrant (Coord x y) = Coord (-signum x * (if abs x >= abs y then 1 else 0)) (-signum y * (if abs y >= abs x then 1 else 0))
