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

palette = brewerSet RdPu 9

resolution = 80

xMin = -2.0
xMax = 2.0
xInc = (xMax - xMin) / resolution
yMin = -2.0
yMax = 2.0
yInc = (yMax - yMin) / resolution

plot :: Diagram B
plot = gridCat' (resolution + 1) $ map drawPoint [(x, y) | y <- ys, x <- xs] where
  drawPoint (x, y) = let d = divergence (x :+ y) in
    if d < iterations
      then square 1 # fc (palette !! (d `mod` length palette)) # lw none
      else square 1 # fc black # lw none 
  xs = [xMin, (xMin + xInc) .. xMax]
  ys = [yMin, (yMin + yInc) .. yMax]
