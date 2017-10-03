{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Mandelbrot

import Control.Parallel.Strategies

import Data.Complex
import Data.Maybe

import Data.Word

import Data.Colour.SRGB (sRGB, toSRGB)
import Data.Colour.RGBSpace
import Data.Colour.Palette.BrewerSet

import Data.ByteString (ByteString, pack, empty, append)
import Data.ByteString.Conversion
import Codec.BMP

-- Number of samples per dimension.
resolution = 400

-- How many samples to calculate in each parallel unit of work.
chunkSize = 200

-- Interesting coordinates.
trident = (-0.1011) :+ 0.9563
armpit = (-0.75) :+ 0

imageLocus = trident
imageScale = 0.05

palette :: [Kolor]
palette = reverse set ++ set where
  set = brewerSet style size
  style = RdPu
  size = 9

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

-- Assign a color to a divergence value in [0..iterations].
colorify :: Int -> Kolor
colorify d = if d < iterations
  then palette !! (d `mod` length palette)
  else sRGB 0.0 0.0 0.0

normalizeColor :: Double -> Word8
normalizeColor val = fromIntegral $ floor (val * 255)

packColor :: Kolor -> ByteString
packColor c = let rgb = toSRGB c
  in pack $ normalizeColor <$> [channelRed rgb, channelGreen rgb, channelBlue rgb, 0]

byteArray :: ByteString
byteArray = foldl append empty $ packColor . colorify <$> divMap samples

main = let bitmap = packRGBA32ToBMP resolution resolution byteArray
  in writeBMP "/dev/stdout" bitmap 
