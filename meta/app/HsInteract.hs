{-# LANGUAGE LambdaCase #-}

import Alien.FFI
import Alien.Galaxy
import Graphics.Gloss.Interface.IO.Game
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Exception
import System.Environment
import System.IO (hPutStrLn, stderr)
import Data.List
import Data.List.Split
import Data.Char (isSpace)

data GameState = GameState
  { uiScale :: Float
  , currentPictures :: [Drawing]
  , currentState :: AlienState
  , mousePos :: (Integer, Integer)
  , showState :: Bool
  , showHttpLog :: Bool
  , httpLog :: [String]
  } deriving (Eq, Ord, Show)

main = do
  initState <- getArgs >>= \case
    [x] -> AlienState <$> evaluate (parseShortList x)
    _   -> pure $ AlienState LNil
  let
    interactor = galaxy

    initGame = GameState
      { uiScale = 5
      , currentPictures = []
      , currentState = initState
      , mousePos = (0, 0)
      , showState = False
      , showHttpLog = False
      , httpLog = []
      }

    mapMouse world (x, y) = (floor $ x / uiScale world + 0.5, floor $ -y / uiScale world + 0.5)

    events (EventMotion mouse) world = pure $ world { mousePos = mapMouse world mouse }
    events (EventKey (MouseButton LeftButton) Down _ mouse) world = do
      putStrLn $ "Clicked " ++ show (mapMouse world mouse)
      runWriterT (makeClick httpSenderLog (lift . printState) interactor (currentState world) (mapMouse world mouse)) >>= \case
        ((state', pics), log) -> pure $ world { currentPictures = pics, currentState = state', httpLog = take 50 $ reverse log ++ httpLog world }
    events (EventKey (SpecialKey KeyDown) Down _ _) world = pure $ world { uiScale = uiScale world * 0.8 }
    events (EventKey (SpecialKey KeyUp) Down _ _) world = pure $ world { uiScale = uiScale world / 0.8 }
    events (EventKey (Char 's') Down _ _) world = pure $ world { showState = not $ showState world }
    events (EventKey (Char 'l') Down _ _) world = pure $ world { showHttpLog = not $ showHttpLog world }
    events (EventKey (Char 'i') Down _ _) world = do
      putStrLn "Input X Y:"
      [x, y] <- map read . words <$> getLine
      putStrLn $ "Clicked " ++ show (x, y)
      hPutStrLn stderr (show x ++ " " ++ show y)
      runWriterT (makeClick httpSenderLog (lift . printState) interactor (currentState world) (x, y)) >>= \case
        ((state', pics), log) -> pure $ world { currentPictures = pics, currentState = state', httpLog = take 50 $ reverse log ++ httpLog world }
    events _ world = pure world

    printState (AlienState state) = putStrLn $ "State: " ++ pprList state

    httpSenderLog req = do
      lift $ putStrLn $ "-> " ++ pprList req
      tell $ reverse $ zipWith (++) ("-> ":repeat "..... ") $ chunksOf 128 $ pprList req
      resp <- lift $ httpSender req
      lift $ putStrLn $ "<- " ++ pprList resp
      tell $ reverse $ zipWith (++) ("<- ":repeat "..... ") $ chunksOf 128 $ pprList resp
      pure resp

    httpSender req = do
      putStrLn $ "Sending " ++ pprList req
      let modReq = map (\case True -> '1'; False -> '0') $ modulate req
      putStrLn $ "Sending raw " ++ modReq
      request <- setRequestBodyLBS (BLU.fromString modReq) <$> parseRequest ("POST https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=5b1e7596cd5446e18dd969e5fcede90b")
      response <- httpLBS request
      case show (getResponseStatusCode response) of
        "200" -> do
          let modResp = BLU.toString $ getResponseBody response
          putStrLn $ "Received raw " ++ show modResp
          let resp = demodulate $ (== '1') <$> modResp
          putStrLn $ "Received " ++ pprList resp
          pure resp
        _ -> error $ "Server error: " ++ show response

    draw world = Pictures $
      [ Scale (uiScale world) (uiScale world) $ drawPictures (currentPictures world)
      , Translate (uiScale world * fromIntegral (fst $ mousePos world)) (uiScale world * negate (fromIntegral (snd $ mousePos world)))
        $ Scale 0.2 0.2 $ Color white $ Text $ show (mousePos world) ]
      ++ if showState world
         then [ Translate (uiScale world * fromIntegral (fst $ mousePos world)) (-70 + uiScale world * negate (fromIntegral (snd $ mousePos world)))
                $ Scale 0.15 0.15 $ Color white $ Pictures [Translate 0 (-200 * i) $ Text line | (i, line) <- zip [0..] $ chunksOf 128 $ pprState $ currentState world] ]
         else []
      ++ if showHttpLog world
         then [ Translate (uiScale world * fromIntegral (fst $ mousePos world)) (-70 + uiScale world * negate (fromIntegral (snd $ mousePos world)))
                $ Scale 0.15 0.15 $ Color white $ Pictures [Translate 0 (-200 * i) $ Text line | (i, line) <- zip [0..] $ httpLog world] ]
         else []
      where
        pprState (AlienState state) = pprList state

    drawPictures pics
      = Pictures $ reverse $ zipWith Color (withAlpha 0.5 <$> cycle colors) (drawPic <$> pics)
      where
        colors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
    drawPic (Drawing coords) = Pictures $ map pixel coords
      where pixel (i, j) = Translate (fromInteger i) (fromInteger (-j)) $ Polygon $ rectanglePath 1 1

  playIO FullScreen black 25 initGame (pure . draw) events (const pure)


data MStruct = MList [MStruct] | MInt Integer | MCons (MStruct, MStruct) deriving (Read, Show)

change :: String -> String
change [] = []
change ('(':xs) = "MCons (" ++ change xs
change ('[':xs) = "MList [" ++ change xs
change list@(x:xs) | x == ')' || x == ']' = x : change xs
                   | isNum x = let (num, rest) = span isNum list
                               in "MInt " ++ num ++ change rest
                   | otherwise = x : change xs
    where isNum y = y `elem` "+-0123456789"

parseMStruct :: String -> MStruct
parseMStruct str = read $ change str'
    where str' = filter (not . isSpace) str

parseShortList :: String -> IntList
parseShortList = toIntList . parseMStruct
    where listToIntList [] = LNil
          listToIntList (x:xs) = LCons (toIntList x) (listToIntList xs)
          toIntList (MInt n) = LInt n
          toIntList (MCons (a, b)) = LCons (toIntList a) (toIntList b)
          toIntList (MList xs) = listToIntList xs

pprList = go []
  where go els LNil = "[" ++ intercalate "," (pprList <$> reverse els) ++ "]"
        go els (LCons x xs) = go (x:els) xs
        go [] (LInt i) = show i
        go els x = "(" ++ intercalate "," (pprList <$> reverse (x:els)) ++ ")"
