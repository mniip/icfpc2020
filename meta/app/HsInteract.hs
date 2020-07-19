{-# LANGUAGE LambdaCase #-}

import Alien.FFI
import Alien.Galaxy
import Graphics.Gloss.Interface.IO.Game hiding (Point)
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
import qualified Data.Set as S
import Data.Array.Unboxed
import Data.Tuple
import Control.Arrow

import Common
import Protocol
import Blocks

data GlossState = GlossState
  { txtScale :: Float
  , uiScale :: Float
  , currentPictures :: [Drawing]
  , currentState :: AlienState
  , mousePos :: (Integer, Integer)
  , showState :: Bool
  , showHttpLog :: Bool
  , httpLog :: [String]
  , showNumbers :: Bool
  } deriving (Eq, Ord, Show)

main = do
  initState <- getArgs >>= \case
    [x] -> AlienState <$> evaluate (parseShortList x)
    _   -> pure $ AlienState LNil
  let
    interactor = galaxy

    initGame = GlossState
      { txtScale = 1
      , uiScale = 5
      , currentPictures = []
      , currentState = initState
      , mousePos = (0, 0)
      , showState = False
      , showHttpLog = False
      , httpLog = []
      , showNumbers = True
      }

    mapMouse world (x, y) = (floor $ x / uiScale world + 0.5, floor $ -y / uiScale world + 0.5)

    events (EventMotion mouse) world = pure $ world { mousePos = mapMouse world mouse }
    events (EventKey (MouseButton LeftButton) Down _ mouse) world = do
      let mouseCoords = mapMouse world mouse
      putStrLn $ "Clicked " ++ show mouseCoords
      hPutStrLn stderr (show (fst mouseCoords) ++ " " ++ show (snd mouseCoords))
      runWriterT (makeClick httpSenderLog (lift . printState) interactor (currentState world) mouseCoords) >>= \case
        ((state', pics), log) -> pure $ world { currentPictures = pics, currentState = state', httpLog = take 50 $ reverse log ++ httpLog world }
    events (EventKey (SpecialKey KeyDown) Down _ _) world = pure $ world { uiScale = uiScale world * 0.8 }
    events (EventKey (SpecialKey KeyUp) Down _ _) world = pure $ world { uiScale = uiScale world / 0.8 }
    events (EventKey (Char 's') Down _ _) world = pure $ world { showState = not $ showState world }
    events (EventKey (Char 'q') Down _ _) world = pure $ world { txtScale = txtScale world * 0.8 }
    events (EventKey (Char 'w') Down _ _) world = pure $ world { txtScale = txtScale world / 0.8 }
    events (EventKey (Char 'l') Down _ _) world = pure $ world { showHttpLog = not $ showHttpLog world }
    events (EventKey (Char 'n') Down _ _) world = pure $ world { showNumbers = not $ showNumbers world }
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
      let preq = maybe (pprList req) show $ (fromProto req :: Maybe Protocol.Request)
      lift $ putStrLn $ "-> " ++ preq
      tell $ reverse $ zipWith (++) ("-> ":repeat "..... ") $ chunksOf 128 preq
      resp <- lift $ httpSender req
      let presp = maybe (pprList resp) show $ (fromProto resp :: Maybe Protocol.Response)
      lift $ putStrLn $ "<- " ++ presp
      tell $ reverse $ zipWith (++) ("<- ":repeat "..... ") $ chunksOf 128 presp
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
      [ Scale (uiScale world) (uiScale world) $ drawPictures (showNumbers world) (currentPictures world)
      , Translate (uiScale world * fromIntegral (fst $ mousePos world)) (uiScale world * negate (fromIntegral (snd $ mousePos world)))
        $ Scale 0.2 0.2 $ Color white $ Text $ show (mousePos world) ]
      ++ if showState world
         then [ Translate (uiScale world * fromIntegral (fst $ mousePos world)) (-70 + uiScale world * negate (fromIntegral (snd $ mousePos world)))
                $ Scale (0.15 * txtScale world) (0.15 * txtScale world) $ Color white $ Pictures [Translate 0 (-200 * i) $ Text line | (i, line) <- zip [0..] $ chunksOf 128 $ pprState $ currentState world] ]
         else []
      ++ if showHttpLog world
         then [ Translate (uiScale world * fromIntegral (fst $ mousePos world)) (-70 + uiScale world * negate (fromIntegral (snd $ mousePos world)))
                $ Scale (0.15 * txtScale world) (0.15 * txtScale world) $ Color white $ Pictures [Translate 0 (-200 * i) $ Text line | (i, line) <- zip [0..] $ httpLog world] ]
         else []
      where
        pprState (AlienState state) = pprList state

    drawPictures showNums pics
      = Pictures $ reverse $ zipWith Color (withAlpha 0.5 <$> cycle colors) (drawPic showNums <$> pics)
      where
        colors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
    drawPic showNums (Drawing coords) = Pictures $ map pixel coords
      ++ if showNums
         then [number n x1 y1 | r@((x1, y1), _) <- findBlocksArr grid, Just n <- [numValue' grid r]]
         else []
      where
        pixel (i, j) = Translate (fromInteger i) (fromInteger (-j)) $ Polygon $ rectanglePath 1 1
        number n i j = Color white $ Translate (-0.5 + fromIntegral i) (-1.5 + fromIntegral (-j)) $ Scale 0.02 0.02 $ Text $ show n

        grid = coordsToArr $ (fromInteger *** fromInteger) <$> coords

  playIO FullScreen black 25 initGame (pure . draw) events (const pure)

coordsToArr :: [Point Int] -> UArray (Int, Int) Bool
coordsToArr coords = accumArray (||) False ((minx, miny), (maxx, maxy)) [(p, True) | p <- coords]
  where
    minx = minimum $ fst <$> nonEmpCoords
    maxx = maximum $ fst <$> nonEmpCoords
    miny = minimum $ snd <$> nonEmpCoords
    maxy = maximum $ snd <$> nonEmpCoords
    nonEmpCoords = case coords of
      [] -> [(0, 0)]
      _  -> coords

findBlocksInArr :: [Point Int] -> UArray (Int, Int) Bool -> [Block Int]
findBlocksInArr rs grid = go S.empty rs
  where
    go seen (p:ps)
      | not $ index p = go seen ps
      | p `S.member` seen = go seen ps
      | r <- findBlock index p
      , nonTrivialBlock r
      = r : go (S.union seen $ S.fromList $ filter index $ range r) ps
      | otherwise
      = go (S.insert p seen) ps
    go seen [] = []

    nonTrivialBlock ((x1, y1), (x2, y2)) = x1 < x2 && y1 < y2

    rng = bounds grid
    index p = inRange rng p && grid ! p

findBlocksArr :: UArray (Int, Int) Bool -> [Block Int]
findBlocksArr = findBlocksInArr =<< (range . bounds)

numValue' :: UArray (Int, Int) Bool -> Block Int -> Maybe Integer
numValue' grid ((x1, y1), (x2, y2))
  | width >= 2 && height >= 2
  , width == height || width + 1 == height
  , all (\x -> grid ! (x, y1)) [x1+1..x2]
  , all (\y -> grid ! (x1, y)) [y1+1..y2]
  , not $ grid ! (x1, y1)
  = Just $ (if width < height then negate else id)
    $ foldr (\x y -> (if x then 1 else 0) + 2 * y) 0
    $ (grid !) <$> (map swap . range . (swap *** swap)) ((x1 + 1, y1 + 1), (x2, y2))
  where
    width = x2 - x1 + 1
    height = y2 - y1 + 1
numValue' _ _ = Nothing
