{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Integration of reanimate and chart-svg
module Chart.Reanimate
  ( ReanimateConfig (..),
    defaultReanimateConfig,
    animChartSvg,
    ChartReanimate (..),
    chartReanimate,
    toTreeA,
    Chart.Reanimate.tree,
    treeFromFile,
  )
where

import Chart as C hiding (Line, box, OriginAbsolute, EllipticalArc, QuadraticBezier, PathCommand (..))
import Codec.Picture.Types
import qualified Data.Attoparsec.Text as A
import Graphics.SvgTree as Svg hiding (Text)
import qualified Graphics.SvgTree.PathParser as Svg
import Prelude
import Reanimate as Re
import Data.Text (Text, unpack)
import GHC.Generics
import Data.Maybe
import Linear.V2 as Linear
import Optics.Core
import qualified Control.Lens as L

-- | Convert from PathInfo to PathCommand
toPathCommand ::
  PathData Double ->
  -- | path text
  PathCommand
toPathCommand (StartP p) = MoveTo OriginAbsolute [toV2 p]
toPathCommand (LineP p) = LineTo OriginAbsolute [toV2 p]
toPathCommand (CubicP c1 c2 p) = CurveTo OriginAbsolute [(toV2 c1, toV2 c2, toV2 p)]
toPathCommand (QuadP c p) = QuadraticBezier OriginAbsolute [(toV2 c, toV2 p)]
toPathCommand (ArcP (ArcInfo (C.Point rx ry) phi' l sw) p) =
  EllipticalArc OriginAbsolute [(rx, ry, phi', l, sw, toV2 p)]

toV2 :: C.Point a -> Linear.V2 a
toV2 (C.Point x y) = Linear.V2 x y

-- | global reanimate configuration.
--
-- >>> defaultReanimateConfig
-- ReanimateConfig {duration = 5.0, background = Just "black", globalFontFamily = Just ["Arial","Helvetica","sans-serif"], globalFontStyle = Just FontStyleNormal, globalAlignment = AlignxMinYMin}
data ReanimateConfig = ReanimateConfig
  { duration :: Double,
    background :: Maybe Text,
    globalFontFamily :: Maybe [Text],
    globalFontStyle :: Maybe Svg.FontStyle,
    globalAlignment :: Svg.Alignment
  }
  deriving (Eq, Show, Generic)

-- |
defaultReanimateConfig :: ReanimateConfig
defaultReanimateConfig = ReanimateConfig 5 (Just "black") (Just ["Arial", "Helvetica", "sans-serif"]) (Just FontStyleNormal) AlignxMinYMin

-- | Animate a ChartSvg animation.
animChartSvg :: ReanimateConfig -> (Double -> ChartSvg) -> Animation
animChartSvg cfg cs =
  mkAnimation (view #duration cfg) $ toTreeA cfg cs

globalAtts :: ReanimateConfig -> Svg.DrawAttributes
globalAtts cfg =
  mempty
    & maybe
      id
      (\x -> fontFamily L..~ Just (fmap unpack x))
      (view #globalFontFamily cfg)
      . maybe
        id
        (\x -> fontStyle L..~ Just x)
        (view #globalFontStyle cfg)

-- | The output of the raw translation of ChartSvg to a reanimate svg tree.
data ChartReanimate = ChartReanimate
  { trees :: [Tree],
    box :: Rect Double,
    size :: C.Point Double
  }
  deriving (Eq, Show, Generic)

-- | Render a 'ChartSvg' to 'Tree's, the fitted chart viewbox, and the suggested SVG dimensions
chartReanimate :: ChartSvg -> ChartReanimate
chartReanimate cs = ChartReanimate ts rect' size'
  where
    cs' = toChartTree cs
    ts = Chart.Reanimate.tree <$> foldOf charts' cs'
    rect' = singletonGuard $ view styleBox' cs'
    C.Point w h = width rect'
    height = view (#svgOptions % #svgHeight) cs
    size' = C.Point (height / h * w) height

-- | convert a ChartSvg animation to a Tree animation.
toTreeA :: ReanimateConfig -> (Double -> ChartSvg) -> Double -> Tree
toTreeA cfg cs x =
  reCss (cs x & view (#svgOptions % #cssOptions)) $
    mkGroup $
      (mkBackground . unpack <$> maybeToList (view #background cfg))
        <> [ ( \cr ->
                 let (Rect x z y w) =
                       box cr
                  in withViewBox'
                       (x, y, z - x, w - y)
                       (PreserveAspectRatio False (view #globalAlignment cfg) Nothing)
                       $ flipYAxis $
                         groupTrees (globalAtts cfg) $ view #trees cr
             )
               $ chartReanimate
                 (cs x)
           ]

reCss :: CssOptions -> (Tree -> Tree)
reCss o =
  Svg.cssApply
  (Svg.cssRulesOfText
   (cssShapeRendering (o ^. #shapeRendering) <>
    cssPreferColorScheme (light, dark) (o ^. #preferColorScheme) <>
    o ^. #cssExtra))

withViewBox' :: (Double, Double, Double, Double) -> Svg.PreserveAspectRatio -> Tree -> Tree
withViewBox' vbox par child =
  Re.translate (- screenWidth / 2) (- screenHeight / 2) $
    svgTree
      Document
        { _documentViewBox = Just vbox,
          _documentWidth = Just (Num screenWidth),
          _documentHeight = Just (Num screenHeight),
          _documentElements = [child],
          _documentDescription = "",
          _documentLocation = "",
          _documentAspectRatio = par
        }

-- | Rectange svg
treeRect :: Rect Double -> Tree
treeRect a =
  RectangleTree $ rectSvg a defaultSvg

-- | Text svg
treeText :: TextStyle -> Text -> C.Point Double -> Tree
treeText s t p =
  TextTree Nothing (textAt (pointSvg p) t)
    & maybe id (\x -> drawAttributes L.%~ rotatePDA x p) (s ^. #rotation)

-- | GlyphShape to svg Tree
treeShape :: GlyphShape -> Double -> C.Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (s / 2))
treeShape SquareGlyph s p = treeRect (move p ((s *) <$> one))
treeShape (RectSharpGlyph x') s p =
  treeRect (move p (C.scale (C.Point s (x' * s)) one))
treeShape (RectRoundedGlyph x'' rx ry) s p =
  RectangleTree
    . rectSvg (addPoint p $ C.scale (C.Point s (x'' * s)) one)
    . (rectCornerRadius L..~ (Just $ Num rx, Just $ Num ry))
    $ defaultSvg
treeShape (TriangleGlyph (C.Point xa ya) (C.Point xb yb) (C.Point xc yc)) s p =
  PolygonTree
    . (polygonPoints L..~ rps)
    $ (drawAttributes L.%~ translateDA p) defaultSvg
  where
    rps =
      [ V2 (s * xa) (- s * ya),
        V2 (s * xb) (- s * yb),
        V2 (s * xc) (- s * yc)
      ]
treeShape (EllipseGlyph x') s p =
  EllipseTree $
    Ellipse
      mempty
      (pointSvg p)
      (Num $ s / 2)
      (Num $ (x' * s) / 2)
treeShape VLineGlyph s (C.Point x y) =
  PolyLineTree
    . (polyLinePoints L..~ [V2 x (-(y-s/2)), V2 x (-(y+s/2))])
    $ defaultSvg
{-
  PolyLineTree
    . (polyLinePoints .~ ((\(C.Point x y) -> V2 x (- y)) <$> xs))
    $ defaultSvg
-}
treeShape HLineGlyph s (C.Point x y) =
  PolyLineTree
    . (polyLinePoints L..~ [V2 (x-s/2) (-y), V2 (x+s/2) (-y)])
    $ defaultSvg
{-
  PolyLineTree
    . (polyLinePoints .~ [V2 x (-(y-s/2)), V2 x (-(y+s/2))])
    $ defaultSvg

-}
treeShape (PathGlyph path _) s p =
  Svg.PathTree
    ( Svg.Path
        ( Svg.defaultSvg
            & (Svg.drawAttributes L.%~ scaleDA (C.Point s s) . translateDA p)
        )
        (either mempty id $ A.parseOnly Svg.pathParser path)
    )

-- | GlyphStyle to svg Tree
treeGlyph :: GlyphStyle -> C.Point Double -> Tree
treeGlyph s p =
  treeShape (s ^. #shape) (s ^. #size) p
    & maybe id (\x -> drawAttributes L.%~ rotatePDA x p) (s ^. #rotation)

-- | line svg
treeLine :: [C.Point Double] -> Tree
treeLine xs =
  PolyLineTree
    . (polyLinePoints L..~ ((\(C.Point x y) -> V2 x (- y)) <$> xs))
    $ defaultSvg

-- | GlyphStyle to svg Tree
treePath :: [PathData Double] -> Tree
treePath s =
  PathTree $
    Path
      mempty
      ( toPathCommand <$> s
      )

-- | convert a 'Chart' to a 'Tree'
tree :: Chart -> Tree
tree (TextChart s xs) =
  groupTrees (daText s) (uncurry (treeText s) <$> xs)
tree (GlyphChart s xs) =
  groupTrees (daGlyph s) (treeGlyph s <$> xs)
tree (LineChart s xs) =
  groupTrees (daLine s) (treeLine <$> xs)
tree (RectChart s xs) =
  groupTrees (daRect s) (treeRect <$> xs)
tree (PathChart s xs) =
  groupTrees (daPath s) [treePath xs]
tree (BlankChart _) = defaultSvg

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  GroupTree (drawAttributes L.%~ (<> da') $ groupChildren L..~ tree' $ defaultSvg)

-- * DrawAttribute computations

daRect :: RectStyle -> DrawAttributes
daRect o =
  mempty
    & (strokeWidth L..~ Just (Num (o ^. #borderSize)))
    & (strokeColor L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #borderColor)))
    & (strokeOpacity L..~ Just (realToFrac (opac $ o ^. #borderColor)))
    & (fillColor L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity L..~ Just (realToFrac (opac $ o ^. #color)))

daText :: () => TextStyle -> DrawAttributes
daText o =
  mempty
    & (fontSize L..~ Just (Num (o ^. #size)))
    & (strokeWidth L..~ Just (Num 0))
    & (strokeColor L..~ Just FillNone)
    & (fillColor L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity L..~ Just (realToFrac (opac $ o ^. #color)))
    & (textAnchor L..~ Just (toTextAnchor $ o ^. #anchor))
  where
    toTextAnchor :: Anchor -> Svg.TextAnchor
    toTextAnchor AnchorMiddle = TextAnchorMiddle
    toTextAnchor AnchorStart = TextAnchorStart
    toTextAnchor AnchorEnd = TextAnchorEnd

daGlyph :: GlyphStyle -> DrawAttributes
daGlyph o =
  mempty
    & (strokeWidth L..~ Just (Num (o ^. #borderSize)))
    & ( strokeColor
          L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #borderColor))
      )
    & (strokeOpacity L..~ Just (realToFrac (opac $ o ^. #borderColor)))
    & (fillColor L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity L..~ Just (realToFrac (opac $ o ^. #color)))
    & maybe id (\(C.Point x y) -> Svg.transform L..~ Just [Translate x (- y)]) (o ^. #translate)

daLine :: LineStyle -> DrawAttributes
daLine o =
  mempty
    & (strokeWidth L..~ Just (Num (o ^. #size)))
    & (strokeColor L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (strokeOpacity L..~ Just (realToFrac (opac $ o ^. #color)))
    & (fillColor L..~ Just FillNone)
    & maybe
      id
      (\x -> strokeLineCap L..~ Just (fromLineCap' x))
      (o ^. #linecap)
    & maybe
      id
      (\x -> strokeLineJoin L..~ Just (fromLineJoin' x))
      (o ^. #linejoin)
    & maybe
      id
      (\x -> strokeOffset L..~ Just (Num x))
      (o ^. #dashoffset)
    & maybe
      id
      (\xs -> strokeDashArray L..~ Just (Num <$> xs))
      (o ^. #dasharray)

fromLineCap' :: LineCap -> Svg.Cap
fromLineCap' LineCapButt = CapButt
fromLineCap' LineCapRound = CapRound
fromLineCap' LineCapSquare = CapSquare

fromLineJoin' :: C.LineJoin -> Svg.LineJoin
fromLineJoin' LineJoinMiter = JoinMiter
fromLineJoin' LineJoinBevel = JoinBevel
fromLineJoin' LineJoinRound = JoinRound

daPath :: PathStyle -> DrawAttributes
daPath o =
  mempty
    & (strokeWidth L..~ Just (Num (o ^. #borderSize)))
    & ( strokeColor
          L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #borderColor))
      )
    & (strokeOpacity L..~ Just (realToFrac (opac $ o ^. #borderColor)))
    & (fillColor L..~ Just (ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity L..~ Just (realToFrac (opac $ o ^. #color)))

-- * svg primitives

-- | Convert to reanimate color primitive.
toPixelRGBA8 :: Colour -> PixelRGBA8
toPixelRGBA8 (Colour r g b o) =
  PixelRGBA8
    (fromIntegral (floor $ r * 256 :: Int))
    (fromIntegral (floor $ g * 256 :: Int))
    (fromIntegral (floor $ b * 256 :: Int))
    (fromIntegral (floor $ o * 256 :: Int))

-- | convert a point to the svg co-ordinate system
-- The svg coordinate system has the y-axis going from top to bottom.
pointSvg :: C.Point Double -> (Svg.Number, Svg.Number)
pointSvg (C.Point x y) = (Num x, Num (- y))

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (HasDrawAttributes s) => Double -> C.Point Double -> s -> s
rotatePDA a (C.Point x y) s = s & Svg.transform L.%~ (Just . maybe r (<> r))
  where
    r = [Rotate (- a * 180 / pi) (Just (x, - y))]

-- | A DrawAttributes to translate by a Point.
translateDA :: (HasDrawAttributes s) => C.Point Double -> s -> s
translateDA (C.Point x' y') =
  Svg.transform
    L.%~ (\x -> Just $ maybe [Translate x' (- y')] (<> [Translate x' (- y')]) x)

-- | A DrawAttributes to translate by a Point.
scaleDA :: (HasDrawAttributes s) => C.Point Double -> s -> s
scaleDA (C.Point x' y') =
  Svg.transform
    L.%~ (\x -> Just $ maybe [Scale x' (Just y')] (<> [Scale x' (Just y')]) x)

-- | convert a Rect to the svg co-ordinate system
rectSvg :: Rect Double -> Svg.Rectangle -> Svg.Rectangle
rectSvg (Rect x z y w) =
  (rectUpperLeftCorner L..~ (Num x, Num (- w)))
    . (rectWidth L..~ Just (Num (z - x)))
    . (rectHeight L..~ Just (Num (w - y)))

-- | import a Tree from a file
treeFromFile :: FilePath -> IO Tree
treeFromFile fp = do
  t <- Svg.loadSvgFile fp
  pure $ maybe Svg.None Re.unbox t
