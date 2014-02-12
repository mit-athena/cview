{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                  (when)
import           Data.List                      (intercalate)
import           Data.String                    (fromString)
import           Text.Blaze.Svg                 (l, m, z)
import           Text.Blaze.Svg.Internal        (appendToPath)
import           Text.Blaze.Svg.Renderer.String (renderSvg)
import           Text.Blaze.Svg11               ((!))
import qualified Text.Blaze.Svg11               as S
import qualified Text.Blaze.Svg11.Attributes    as A

main :: IO ()
main = do
  putStrLn . renderSvg $ circleSvg 81 100

circleSvg :: Integer -- Current occupancy
          -> Integer -- Maximum occupancy
          -> S.Svg
circleSvg cur max = S.docTypeSvg ! A.version "1.1" ! A.width "100" ! A.height "100" ! A.viewbox "0 0 100 100" $ do
  -- Green segment
  when (cur > 0) $ S.path ! A.d (arcPath 0 (fromInteger cur / fromInteger max)) ! A.fill "#00FF00"
  -- Red segment
  when (cur < max) $ S.path ! A.d (arcPath (fromInteger cur / fromInteger max) 1) ! A.fill "#FF0000"
  -- White circle
  S.circle ! A.cx "50" ! A.cy "50" ! A.r "40" ! A.fill "#FFFFFF"
  -- Number overlay
  S.text_ ! A.y "60" ! A.x "20" ! A.fontSize "50" $ fromString $ show cur

-- | Elliptical Arc
a :: Show a => a -> a -> a -> a -> a -> a -> a -> S.Path
a rx ry xar laf sf ex ey = appendToPath
  [ "A "
  , show rx, ", "
  , show ry, ", "
  , show xar, ", "
  , show laf, ", "
  , show sf, ", "
  , show ex, ", "
  , show ey, " "
  ]

arcPath :: Double
        -> Double
        -> S.AttributeValue
arcPath start end = S.mkPath $ do
  let large_arc_flag = if (end - start) < 0.5 then 0 else 1
      sweep_flag = 1
      rx = 50
      ry = 50
      x_axis_rotation = 0
      start_x = 50 + (50 * sin (2*pi*start))
      start_y = 50 - (50 * cos (2*pi*start))
      end_x = 50 + (50 * sin (2*pi*end))
      end_y = 50 - (50 * cos (2*pi*end))
  m start_x start_y
  a rx ry x_axis_rotation large_arc_flag sweep_flag end_x end_y
  l 50 50
  z

