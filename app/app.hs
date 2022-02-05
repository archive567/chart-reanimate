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
main = reanimate $ setDuration 10 $ (animChartSvg defaultReanimateConfig $ \x -> merge [(\x -> wheel 0.05 200 x 0.6 & (#hudOptions %~ colourHudOptions (rgb light)) & over #charts (<> named "setrange" [BlankChart [Rect (-0.33) 0.33 (-0.33) 0.33]])), countChart] x)

{-
  reanimate $
    foldl' seqA (pause 0) $
    (applyE (overBeginning 1 fadeInE) .
     applyE (overEnding 1 fadeOutE)) .
    mapA pathify .
    (\cs -> animChartSvg defaultReanimateConfig (const cs)) .
    (#hudOptions %~ colourHudOptions (const light)) <$> examples

-}

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
    arcFlagsExample,
    quadExample,
    cubicExample,
    pathExample,
    vennExample,
    arrowExample
  ]

-- | Color wheel displaying palette1 choices
--
-- -- ![wheel example](other/wheel.svg)
wheelLightness :: Double -> ChartSvg
wheelLightness x = dotMap 0.01 50 x 0.5 (palette1 <$> [0 .. 7])


-- (wheel 0.05 200 0.3 0.6)

-- | Color wheel displaying palette1 choices
--
-- -- ![wheel example](other/wheel.svg)
wheelExample :: ChartSvg
wheelExample = dotMap 0.01 50 0.5 0.5 (palette1 <$> [0 .. 7])

-- | Counter HUD
--
countChart :: Double -> ChartSvg
countChart x = mempty & #charts .~ unnamed [TextChart (defaultTextStyle & #size .~ 0.08 & #color .~ light) [(fixed (Just 2) x, Point 0.3 0.3)]]

merge :: [Double -> ChartSvg] -> Double -> ChartSvg
merge xs x = mconcat $ (\f -> f x) <$> xs

-- | The dotMap
--
-- > dotMap 0.01 20 0.8 0.3
dotMap :: Double -> Int -> Double -> Double -> [Colour] -> ChartSvg
dotMap s grain l maxchroma cs =
  mempty
    & #hudOptions
    .~ defaultHudOptions
    & #charts
    .~ named "dots" (dot_ <$> cs)
    <> named
      "wheel"
      ( ( \(p, c) ->
            GlyphChart
              ( defaultGlyphStyle
                  & #size .~ s
                  & #color .~ c
                  & #borderSize .~ 0
              )
              [p]
        )
          <$> filter (validColour . snd) (wheelPoints grain l maxchroma)
      )

dot_ :: Colour -> Chart
dot_ x = (\(p, c) -> GlyphChart (defaultGlyphStyle & #size .~ 0.08 & #color .~ c & #borderColor .~ Colour 0.5 0.5 0.5 1 & #shape .~ CircleGlyph) [p]) (colour2Point x, x)
  where
    colour2Point c = review lcha2colour' c & (\(LCHA _ ch h _) -> uncurry Point (review xy2ch' (ch, h)))

wheelPoints :: Int -> Double -> Double -> [(Point Double, Colour)]
wheelPoints grain l maxchroma =
  (\(Point c h) -> (uncurry Point $ view (re xy2ch') (c, h), view lcha2colour' (LCHA l c h 1)))
    <$> grid LowerPos (Rect 0 maxchroma 0 360) (Point grain grain)

wheel :: Double -> Int -> Double -> Double -> ChartSvg
wheel s grain l maxchroma =
  mempty &
  #hudOptions .~ defaultHudOptions &
  #charts .~ named "wheel"
    ((\(p,c) -> GlyphChart
       (defaultGlyphStyle &
        #size .~ s &
        #color .~ c &
        #borderSize .~ 0) [p]) <$>
     filter (validColour . snd) (wheelPoints grain l maxchroma))
