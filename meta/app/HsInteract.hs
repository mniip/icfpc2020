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

import Blocks

data GameState = GameState
  { uiScale :: Float
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

    initGame = GameState
      { uiScale = 5
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
      lift $ putStrLn $ "-> " ++ showInterpret req
      tell $ reverse $ zipWith (++) ("-> ":repeat "..... ") $ chunksOf 128 $ showInterpret req
      resp <- lift $ httpSender req
      lift $ putStrLn $ "<- " ++ showInterpret resp
      tell $ reverse $ zipWith (++) ("<- ":repeat "..... ") $ chunksOf 128 $ showInterpret resp
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
                $ Scale 0.15 0.15 $ Color white $ Pictures [Translate 0 (-200 * i) $ Text line | (i, line) <- zip [0..] $ chunksOf 128 $ pprState $ currentState world] ]
         else []
      ++ if showHttpLog world
         then [ Translate (uiScale world * fromIntegral (fst $ mousePos world)) (-70 + uiScale world * negate (fromIntegral (snd $ mousePos world)))
                $ Scale 0.15 0.15 $ Color white $ Pictures [Translate 0 (-200 * i) $ Text line | (i, line) <- zip [0..] $ httpLog world] ]
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

type Id = Integer

data Command = Detonate Id |
               Unk IntList |
               Move Id (Integer, Integer) | -- Id; Movement direction (x, y), x = -2..2, y = -2..2
               Fire Id (Integer, Integer) Integer | -- Id; Target coords; Energy (3 lines)
               Spawn Id Integer Integer Integer Integer -- Id; Stats (HP, mana, charismata, telomeres)
data Requests = DoThis Integer [[Command]] deriving (Show)

instance Show Command where
    show (Detonate i) = show i ++ " detonates"
    show (Move i v) = show i ++ " moves by " ++ show v
    show (Fire i v e) = show i ++ " fires at " ++ show v ++ " " ++ show e
    show (Spawn i hp mn ch tl) = show i ++ " spawns " ++
                                 "HP: " ++ show hp ++ ", MN: " ++ show mn ++ ", CH: " ++ show ch ++ ", TL: " ++ show tl
    show (Unk list) = pprList list

tryList :: IntList -> Maybe [IntList]
tryList LNil = return []
tryList (LCons x xs) = do
    xs' <- tryList xs
    return (x : xs')

tryCommand :: IntList -> Command
tryCommand (LCons (LInt 0) (LCons (LInt i) (LCons (LCons (LInt x) (LInt y)) LNil))) = Move i (x, y)
tryCommand (LCons (LInt 1) (LCons (LInt i) LNil)) = Detonate i
tryCommand (LCons (LInt 2) (LCons (LInt i) (LCons (LCons (LInt x) (LInt y)) (LCons (LInt e) LNil)))) = Fire i (x, y) e
tryCommand (LCons
             (LInt 3)
             (LCons
               (LInt i)
               (LCons
                 (LCons
                   (LInt hp)
                   (LCons
                     (LInt mana)
                     (LCons
                       (LInt ch)
                       (LCons (LInt telo) LNil)
                     )
                   )
                 )
               LNil))) =
    Spawn i hp mana ch telo
tryCommand list = Unk list

tryInterpret (LCons (LInt 4) (LCons (LInt key) list)) = do
    bots <- tryList list
    cmds <- mapM tryList bots
    let cmdint = map (map tryCommand) cmds
    return (DoThis key cmdint)
tryInterpret ls = Nothing

showInterpret ls = case tryInterpret ls of
                       Just r -> show r
                       Nothing -> pprList ls
