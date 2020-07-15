{-# LANGUAGE DeriveFunctor #-}

import Data.Binary.Get
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.List
import Data.Complex
import Data.Ratio
import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | Numerically integrates the signal with sine and cosine of a fixed frequency
-- to find the amplitude/phase of that frequency in the signal.
trigTransform
  :: Double -- ^ Wave frequency, full periods per sample
  -> Rational -- ^ Intended signal length, in samples
  -> [Double] -- ^ Piece of the signal
  -> Double -- ^ "Fine tune": a fraction of a sample by which the signal should
            -- be shifted back
  -> Complex Double -- ^ Re = sine integral, Im = cosine integral
trigTransform freq len signal finetune =
  sum $ zipWith (\i s -> scale (s * dx) $ sic $ omega * (fromIntegral i + finetune)) [0::Int ..] signal
  where
    sic theta = sin theta :+ cos theta
    scale s (x :+ y) = s * x :+ s * y
    dx = 2 / fromRational len
    omega = 2 * pi * freq

-- | Split signal into chunks and apply 'trigTransform' to each, on multiple
-- frequencies.
trigTransformChunks
  :: (i -> Double) -- ^ Wave frequencies, full periods per sample
  -> [Double] -- ^ Input signal
  -> Rational -- ^ Length of one chunk in samples
  -> [i -> Complex Double] -- ^ Re = sine integral, Im = cosine integral
trigTransformChunks freqs signal size = go 0 signal
  where
    go _ [] = []
    go finetune signal =
      fmap (\freq -> trigTransform freq size current (fromRational finetune)) freqs
      : go (frac cutoff) next
      where
        cutoff :: Rational
        cutoff = size + finetune
        current = take (ceiling cutoff) signal
        next = drop (floor cutoff) signal
        frac r = (numerator r `mod` denominator r) % denominator r

getPCM :: Get [Double]
getPCM = many $ (\sample -> fromIntegral sample / 0x8000) <$> getInt16le

readPCM :: String -> IO [Double]
readPCM filename = runGet getPCM <$> LBS.readFile filename

getWAV :: Get [Double]
getWAV = do
  riff <- getWord32le
  guard $ riff == 0x46464952
  _ <- getWord32le
  wave <- getWord32le
  guard $ wave == 0x45564157
  fmt <- getWord32le
  guard $ fmt == 0x20746d66
  subch <- getWord32le
  format <- getWord16le
  guard $ format == 1
  channels <- getWord16le
  guard $ channels == 1
  _sampleRate <- getWord32le
  _byteRate <- getWord32le
  _blockAlign <- getWord16le
  depth <- getWord16le
  guard $ depth == 16
  skip $ fromIntegral subch - 16
  datach <- getWord32le
  guard $ datach == 0x61746164
  _ <- getWord32le
  getPCM

readWAV :: String -> IO [Double]
readWAV filename = runGet getWAV <$> LBS.readFile filename

guessCutoff :: [Double] -> Double
guessCutoff xs = average $ drop lo $ take hi $ sort xs
  where
    len = length xs
    lo = floor (fromIntegral len * 0.05)
    hi = ceiling (fromIntegral len * 0.95)
    average xs = sum xs / fromIntegral (length xs)

defaultFrequencies :: Bool -> Double
defaultFrequencies False = 500.0 / 44100
defaultFrequencies True = 600.0 / 44100

defaultSize :: Rational
defaultSize = 0.05 * 44100

translateModulation
  :: (Enum i, Bounded i)
  => [i -> Complex Double]
  -> [Either [i] i] -- ^ Right means there's a single prevalent frequency,
                    -- Left means there's multiple or no frequencies.
translateModulation xs = map (\m -> single (\i -> m i > cutoff)) mags
  where
    mags = map (magnitude .) xs
    cutoff = guessCutoff $ liftA2 ($) mags [minBound..maxBound]
    single p = case filter p [minBound..maxBound] of
                 [x] -> Right x
                 xs  -> Left xs

demodulate :: [Double] -> [Bool]
demodulate signal = mapMaybe ok $ translateModulation
  $ trigTransformChunks defaultFrequencies signal defaultSize
  where
    ok (Right b) = Just b
    ok (Left []) = Nothing
    ok (Left bs) = error $ "Frequency conflict: " ++ show bs

wrap :: Int -> [Bool] -> IO ()
wrap i xs = mapM_ (putStrLn . map letter) $ chunksOf i xs
  where
    letter True  = '█'
    letter False = ' '
    chunksOf _ [] = []
    chunksOf i xs | (hs, ts) <- splitAt i xs = hs : chunksOf i ts

guessWrap :: [Bool] -> Int
guessWrap xs = length (takeWhile id xs) - 1

main = (wrap =<< guessWrap) =<< demodulate <$> readWAV "/dev/stdin"
