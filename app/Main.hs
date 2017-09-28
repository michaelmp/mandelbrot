{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Mandelbrot

import Data.Complex
import Data.Maybe

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Grid

import Data.Colour (withOpacity)
import Data.Colour.Palette.Harmony
import Data.Colour.Palette.BrewerSet

main = mainWith plot

palette = brewerSet style size ++ reverse (brewerSet style size) where
  size = 11
  style = PiYG

resolution = 200

imageLocus = (-0.1011) :+ 0.9563
imageScale = 0.05

xMin = realPart imageLocus - imageScale
xMax = realPart imageLocus + imageScale
xInc = (xMax - xMin) / resolution
yMin = imagPart imageLocus - imageScale
yMax = imagPart imageLocus + imageScale
yInc = (yMax - yMin) / resolution

plot :: Diagram B
plot = gridCat' (resolution + 1) $ map drawPoint [(x, y) | y <- ys, x <- xs] where
  drawPoint (x, y) = let d = divergence (x :+ y) in
    if d < iterations
      then square 1 # fc (palette !! (d `mod` length palette)) # lw none
      else square 1 # fc black # lw none 
  xs = [xMin, (xMin + xInc) .. xMax]
  ys = [yMin, (yMin + yInc) .. yMax]
