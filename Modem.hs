{-# LANGUAGE DeriveFunctor, BangPatterns #-}

module Modem where

import Data.Binary.Get
import Data.Binary.Put
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
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
  sum $ zipWith (\i s -> mkPolar (s * dx) $ omega * (fromIntegral i + finetune)) [0::Int ..] signal
  where
    dx = 2 / fromRational len
    omega = 2 * pi * freq

frac :: Integral a => Ratio a -> Ratio a
frac r = (numerator r `mod` denominator r) % denominator r

-- | Split signal into chunks and apply 'trigTransform' to each, on multiple
-- frequencies.
trigTransformChunks
  :: (Bounded i, Enum i)
  => (i -> Double) -- ^ Wave frequencies, full periods per sample
  -> [Double] -- ^ Input signal
  -> Rational -- ^ Length of one chunk in samples
  -> [i -> Complex Double] -- ^ Re = sine integral, Im = cosine integral
trigTransformChunks freqs signal size = go 0 signal
  where
    go _ [] = []
    go finetune signal =
      ((memo !!) . fromEnum) : go (frac cutoff) next
      where
        cutoff :: Rational
        cutoff = size + finetune
        current = take (ceiling cutoff) signal
        next = drop (floor cutoff) signal
        memo = (\i -> trigTransform (freqs i) size current (fromRational finetune))
          <$> [minBound..maxBound]

getPixmap :: Get [Bool]
getPixmap = many $ (/= 0) <$> getWord8

readPixmap :: String -> IO [Bool]
readPixmap filename = runGet getPixmap <$> LBS.readFile filename

getPCM :: Get [Double]
getPCM = many $ (\sample -> fromIntegral sample / 0x8000) <$> getInt16le

putPCM :: [Double] -> Put
putPCM xs = mapM_ putSample xs
  where
    putSample s = putWord16le $ floor $ max (-0x8000) $ min 0x7FFF $ s * 0x8000

readPCM :: String -> IO [Double]
readPCM filename = runGet getPCM <$> LBS.readFile filename

writePCM :: String -> [Double] -> IO ()
writePCM filename xs = LBS.writeFile filename $ runPut $ putPCM xs

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

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

defaultFrequencies :: Bool -> Double
defaultFrequencies False = 500.0 / 44100
defaultFrequencies True = 600.0 / 44100

defaultSize :: Rational
defaultSize = 0.05 * 44100

translateModulation'
  :: (Enum i, Bounded i)
  => [i -> Complex Double]
  -> ([Either [i] i], Double)
                    -- ^ Right means there's a single prevalent frequency,
                    -- Left means there's multiple or no frequencies.
                    -- The second component is the average amplitude of
                    -- active frequencies
translateModulation' xs = (map (\m -> single (\i -> m i > cutoff)) mags, avg)
  where
    mags = map (magnitude .) xs
    allAmps = liftA2 ($) mags [minBound..maxBound]
    cutoff = guessCutoff allAmps
    single p = case filter p [minBound..maxBound] of
                 [x] -> Right x
                 xs  -> Left xs
    avg = average $ filter (> cutoff) allAmps

demodulate' :: [Double] -> ([Bool], Double, Int)
  -- ^ Return decoded data, average amplitude, initial delay in chunks
demodulate' signal = (mapMaybe ok decoded, avg, length $ takeWhile (== Left []) decoded)
  where
    ok (Right b) = Just b
    ok (Left []) = Nothing
    ok (Left bs) = error $ "Frequency conflict: " ++ show bs
    (decoded, avg) = translateModulation'
      $ trigTransformChunks defaultFrequencies signal defaultSize

demodulate :: [Double] -> [Bool]
demodulate signal = case demodulate' signal of (xs, _, _) -> xs

emitSine
  :: Double -- ^ Wave frequency, full periods per sample
  -> Int -- ^ Signal length
  -> Double -- ^ Phase, number of samples by which the signal should
            -- be shifted back
  -> Double -- ^ Amplitude
  -> [Double]
emitSine freq len finetune amp
  = map (\i -> amp * sin (omega * (fromIntegral i - finetune))) [0..len - 1]
  where
    omega = 2 * pi * freq

emitSineChunks
  :: (i -> Double) -- ^ Frequency mapping
  -> (Int -> Double) -- ^ Phase mapping
  -> Double -- ^ Amplitude
  -> Rational -- ^ Length of one chunk in samples
  -> [i]
  -> [Double]
emitSineChunks freqs phases amp size xs = go 0 0 xs
  where
    go !i !pos [] = []
    go !i !pos (x:xs)
      = emitSine (freqs x) (ceiling new - ceiling pos) (phases i) amp
        ++ go (i + 1) new xs
      where
        new = pos + size

pflockingenDelay :: Rational
pflockingenDelay = 20 * defaultSize

pegovkaDelay :: Rational
pegovkaDelay = 40 * defaultSize

pflockingenPhases :: Int -> Double
pflockingenPhases i = if (i == 1 || i == 2) || (i >= 83 && i < 718) || (i >= 1720 && i < 2887) || (i >= 6376 && i < 12060)
  then 1.0
  else 0.0

pegovkaPhases :: Int -> Double
pegovkaPhases i = 0.0

modulate :: [Bool] -> [Double]
modulate xs
  = replicate (floor pflockingenDelay) 0.0
    ++ emitSineChunks defaultFrequencies pflockingenPhases 1.0 defaultSize xs

cleanse :: [Double] -> [Double]
cleanse signal = case demodulate' signal of
  (decoded, avg, delay) ->
    let clean = replicate (floor $ fromIntegral delay * defaultSize) 0.0
                ++ emitSineChunks defaultFrequencies pflockingenPhases avg defaultSize decoded
      in zipWith subtract (clean ++ repeat 0.0) signal

movingAvg :: Int -> [Double] -> [Double]
movingAvg n = map ((/ fromIntegral n) . sum . take n) . tails

shrink :: [Double] -> [Double]
shrink = map average . chunksOf 100

diff :: Double -> [Double] -> [Double]
diff freq = zipWith3 amp <$> id <*> drop dt <*> drop (2 * dt)
  where
    dt = 30
    amp a b c = sqrt $ (d / omega)^2 + (d2 / omega^2)^2
      where
        d = b - a
        d2 = a + c - 2 * b
        omega = 2 * pi * freq * fromIntegral dt
