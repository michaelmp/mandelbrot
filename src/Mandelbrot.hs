module Mandelbrot where

import Data.Complex
import Data.Maybe

type Point = Complex Double

iterations = 100

mandelbrot :: Point -> Point -> Point
mandelbrot orbit accumulation = accumulation ^^ 2 + orbit

series :: Point -> [Point]
series c = iterate (mandelbrot c) (0 :+ 0)

escaped :: Point -> Bool
escaped c = 4 < (realPart c * realPart c + imagPart c * imagPart c)

-- Does the series at 'c' eventually escape?
divergent :: Point -> Bool
divergent c = any escaped (take iterations (series c))

-- How many iterations does it take the series at 'c' to escape?
divergence :: Point -> Int
divergence c = helper 0 (0.0 :+ 0.0) where
  helper n acc = if n < iterations
    then
      let c' = mandelbrot c acc in
        if escaped c' then n else helper (n + 1) c'
    else
      n
