{-# LANGUAGE LambdaCase #-}

import Alien.FFI
import Alien.Galaxy
import Graphics.Gloss.Interface.IO.Game
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception
import System.Environment
import Data.List
import Data.List.Split

data GameState = GameState
  { uiScale :: Float
  , currentPictures :: [Drawing]
  , currentState :: AlienState
  , mousePos :: (Integer, Integer)
  , showState :: Bool
  } deriving (Eq, Ord, Show)

main = do
  initState <- getArgs >>= \case
    [x] -> evaluate $ read x
    _   -> pure $ AlienState LNil
  let
    interactor = galaxy

    initGame = GameState { uiScale = 5, currentPictures = [], currentState = initState, mousePos = (0, 0), showState = False }

    mapMouse world (x, y) = (floor $ x / uiScale world + 0.5, floor $ -y / uiScale world + 0.5)

    events (EventMotion mouse) world = pure $ world { mousePos = mapMouse world mouse }
    events (EventKey (MouseButton LeftButton) Down _ mouse) world = do
      putStrLn $ "Clicked " ++ show (mapMouse world mouse)
      makeClick httpSender printState interactor (currentState world) (mapMouse world mouse) >>= \case
        (state', pics) -> pure $ world { currentPictures = pics, currentState = state' }
    events (EventKey (SpecialKey KeyDown) Down _ _) world = pure $ world { uiScale = uiScale world * 0.8 }
    events (EventKey (SpecialKey KeyUp) Down _ _) world = pure $ world { uiScale = uiScale world / 0.8 }
    events (EventKey (Char 's') Down _ _) world = pure $ world { showState = not $ showState world }
    events (EventKey (Char 'i') Down _ _) world = do
      putStrLn "Input X Y:"
      [x, y] <- map read . words <$> getLine
      putStrLn $ "Clicked " ++ show (x, y)
      makeClick httpSender printState interactor (currentState world) (x, y) >>= \case
        (state', pics) -> pure $ world { currentPictures = pics, currentState = state' }
    events _ world = pure world

    printState state = putStrLn $ "State: " ++ show state

    httpSender req = do
      putStrLn $ "Sending " ++ show req
      let modReq = map (\case True -> '1'; False -> '0') $ modulate req
      putStrLn $ "Sending raw " ++ modReq
      request <- setRequestBodyLBS (BLU.fromString modReq) <$> parseRequest ("POST https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=5b1e7596cd5446e18dd969e5fcede90b")
      response <- httpLBS request
      case show (getResponseStatusCode response) of
        "200" -> do
          let modResp = BLU.toString $ getResponseBody response
          putStrLn $ "Received raw " ++ show modResp
          let resp = demodulate $ (== '1') <$> modResp
          putStrLn $ "Received " ++ show resp
          pure resp
        _ -> error $ "Server error: " ++ show response

    draw world = Pictures $
      [ Scale (uiScale world) (uiScale world) $ drawPictures (currentPictures world)
      , Translate (uiScale world * fromIntegral (fst $ mousePos world)) (uiScale world * negate (fromIntegral (snd $ mousePos world)))
        $ Scale 0.2 0.2 $ Color white $ Text $ show (mousePos world)
      ] ++ if showState world
           then [ Translate (uiScale world * fromIntegral (fst $ mousePos world)) (-70 + uiScale world * negate (fromIntegral (snd $ mousePos world)))
                  $ Scale 0.15 0.15 $ Color white $ Text $ unlines $ chunksOf 100 $ pprState $ currentState world ]
           else []
      where
        pprState (AlienState state) = pprList state
        pprList = go []
          where go els LNil = "[" ++ intercalate "," (pprList <$> reverse els) ++ "]"
                go els (LCons x xs) = go (x:els) xs
                go [] (LInt i) = show i
                go els x = "(" ++ intercalate "," (pprList <$> reverse (x:els)) ++ ")"

    drawPictures pics
      = Pictures $ reverse $ zipWith Color (withAlpha 0.5 <$> cycle colors) (drawPic <$> pics)
      where
        colors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
    drawPic (Drawing coords) = Pictures $ map pixel coords
      where pixel (i, j) = Translate (fromInteger i) (fromInteger (-j)) $ Polygon $ rectanglePath 1 1

  playIO FullScreen black 25 initGame (pure . draw) events (const pure)
