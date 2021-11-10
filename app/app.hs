#!/usr/bin/env stack
-- stack runghc --package reanimate

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | reanimate example
--
-- To run this example:
--
-- stack runghc --package reanimate app/reanimate-example.hs
--
-- or
--
-- cabal exec -- runhaskell app/app.hs
--
-- and wait for the browser to open ...
module Main where

import Chart
import Chart.Examples
import Chart.Reanimate
import NumHask.Prelude hiding (fold)
import Reanimate as Re
import Optics.Core

main :: IO ()
main =
  reanimate $
    foldl' seqA (pause 0) $ (applyE (overBeginning 1 fadeInE) . applyE (overEnding 1 fadeOutE)) . mapA pathify . (\cs -> animChartSvg defaultReanimateConfig (const cs)) . (#hudOptions %~ colourHudOptions (const light)) <$> examples

examples :: [ChartSvg]
examples =
  [ unitExample,
    hudOptionsExample,
    rectExample,
    textExample,
    glyphsExample,
    lineExample,
    barExample,
    waveExample,
    surfaceExample,
    arcExample,
    arcFlagsExample,
    ellipseExample,
    quadExample,
    cubicExample,
    pathExample,
    vennExample,
    arrowExample
  ]
