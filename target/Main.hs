{-# LANGUAGE LambdaCase #-}
import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception
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
  [uri, skey] <- getArgs
  let key = GameId $ read skey
  resp1 <- runHTTP uri $ ReqJoin key [103652820,192496425430]
  runHTTP uri (ReqStart key (Just $ Stats 8 8 0 1)) >>= go uri key
  where
    go uri key (RespGame Finished _ _) = pure ()
    go uri key (RespGame Started info status) = do
      runHTTP uri (ReqAct key []) >>= go uri key

