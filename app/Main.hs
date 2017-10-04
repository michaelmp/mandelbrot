{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Mandelbrot

import Control.Parallel.Strategies

import Data.Complex
import Data.Maybe
import Data.List (foldl')
import Data.Word

import Data.Colour.SRGB (sRGB, toSRGB)
import Data.Colour.RGBSpace
import Data.Colour.Palette.BrewerSet

import Data.ByteString (ByteString, pack, concat)
import Data.ByteString.Conversion
import Codec.BMP

-- Number of samples per dimension.
resolution = 800

-- How many samples to calculate in each parallel unit of work.
chunkSize = 100

-- Useful coordinates.
starburst = (-0.3740) :+ 0.6596
trident = (-0.1011) :+ 0.9563
seahorse = (-0.75) :+ 0.01
origin = 0 :+ 0

imageLocus = seahorse 
imageScale = 0.05

palette :: [Kolor]
palette = let
  style = Spectral
  size = 9
  in brewerSet style size

xMin = realPart imageLocus - imageScale
xMax = realPart imageLocus + imageScale
xInc = (xMax - xMin) / resolution
yMin = imagPart imageLocus - imageScale
yMax = imagPart imageLocus + imageScale
yInc = (yMax - yMin) / resolution

samples = [x :+ y | y <- ys, x <- xs] where
  xs = tail [xMin, (xMin + xInc) .. xMax]
  ys = tail [yMin, (yMin + yInc) .. yMax]

divMap :: [Complex Double] -> [Int]
divMap = withStrategy (parListChunk chunkSize rpar) . map divergence

normalizeDiv :: Int -> Double
normalizeDiv d = fromIntegral d / 100 --fromIntegral iterations

-- Assign a color to a divergence value in [0..iterations].
colorify :: Int -> Kolor
colorify d = let cap = length palette in
  if d < iterations
  then
    let intensity = min (normalizeDiv d) 1
    in sRGB intensity 0.0 intensity 
  else sRGB 0.0 0.0 0.0

normalizeColor :: Double -> Word8
normalizeColor val = fromIntegral $ floor (val * 256)

packColor :: Kolor -> ByteString
packColor c = let rgb = toSRGB c
  in pack $ normalizeColor <$> [channelRed rgb, channelGreen rgb, channelBlue rgb, 256]

byteArray :: ByteString
byteArray = Data.ByteString.concat (packColor . colorify <$> divMap samples)

main = let bitmap = packRGBA32ToBMP resolution resolution byteArray
  in writeBMP "/dev/stdout" bitmap 
