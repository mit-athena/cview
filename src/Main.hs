{- Copyright (C) 2014  Jesse Selover <jselover@mit.edu> -}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative            ((<*>))
import           Control.Monad                  (void, when)
import           Data.List                      (foldl1', intercalate)
import           Data.String                    (fromString)
import           System.Process                 (readProcess)
import           Text.Blaze.Svg                 (l, m, z)
import           Text.Blaze.Svg.Internal        (appendToPath)
import           Text.Blaze.Svg.Renderer.String (renderSvg)
import           Text.Blaze.Svg11               ((!))
import qualified Text.Blaze.Svg11               as S
import qualified Text.Blaze.Svg11.Attributes    as A
import           Text.XML.HXT.Core              hiding (when)

locations :: [(Integer, Integer)]
locations = [ (631, 528) -- Barker
            , (818, 774) -- 4
            , (1024, 968) -- Hayden
            , (742, 280) -- 38
            , (1151, 530) -- 56
            , (1343, 551) -- 66
            , (1956, 1008) -- 51
            , (424, 652) -- Rotch
            , (153, 481) -- w20
            ]

-- | bimap (+)
mergeOccupancies :: (Integer, Integer)
                 -> (Integer, Integer)
                 -> (Integer, Integer)
mergeOccupancies (a, b) (c, d) = (a+c, b+d)

-- | Partial coercion function
getInts :: [String] -> (Integer, Integer)
getInts (a:b:[]) = (read a, read b)

main :: IO ()
main = do
  occupanciesStr <- readProcess "./larvnet.sh" [] []
  let ocLines = lines occupanciesStr
      occupanciesLists = map (\l -> [(!! 4), (!! 5)] <*> [words l]) ocLines -- Can error
      occupanciesPreBarker = map getInts occupanciesLists -- Can error
      occupancies = map (freCurToCurMax) $ mergeOccupancies (head occupanciesPreBarker) (head . tail $ occupanciesPreBarker) : (tail . tail $ occupanciesPreBarker) -- Can error
  putStrLn $ show occupanciesLists
  putStrLn $ show occupancies
  void $ runX ( readDocument [] "base.svg"
                >>>
                (foldl1' (>>>) (map cAddCircle $ zip occupancies locations))
                >>>
                writeDocument [] "cview.svg"
              )
  where
    freCurToCurMax :: (Integer, Integer) -> (Integer, Integer)
    freCurToCurMax (fre, cur) = (cur, fre+cur)

-- | Curried addCircle
cAddCircle :: (ArrowXml a, Show s) => ((Integer, Integer), (s, s)) -> a XmlTree XmlTree
cAddCircle ((a, b), (c, d)) = addCircle a b c d

-- | Produces an arrow which adds a circle to an svg file
addCircle :: (ArrowXml a, Show s) => Integer -- Current occupancy
          -> Integer -- Maximum occupancy
          -> s -- x coord
          -> s -- y coord
          -> a XmlTree XmlTree
addCircle cur max x y = processTopDownUntil $ (hasName "svg") `guards`
            (replaceChildren ( getChildren
                               <+>
                               ( parseBlaze (circleSvg cur max)
                                 >>>
                                 adjustPlacement x y 0.7 0.7
                               )
                             )
            )

-- | A helper function for transform
adjustPlacement :: (ArrowXml a, Show p, Show s) => p -> p -> s -> s -> a XmlTree XmlTree
adjustPlacement x y sx sy = transform $ "translate(" ++ show x ++ "," ++ show y ++ ") " ++
                                        "scale(" ++ show sx ++ "," ++ show sy ++ ")"

-- | An arrow that applies a transformation to the current node
transform :: (ArrowXml a) => String -> a XmlTree XmlTree
transform s = addAttrl (sattr "transform" s)

-- | Convert a blaze representation to an arrow which produces an HXT
-- representation
parseBlaze :: (ArrowXml a) => S.Svg -> a b XmlTree
parseBlaze svg = arr (const $ renderSvg svg)
                 >>>
                 xread

-- | Circle fragment
circleSvg :: Integer -- Current occupancy
          -> Integer -- Maximum occupancy
          -> S.Svg
circleSvg cur max = S.g $ do -- no doctype
  -- (will fail at extremes, because elliptical arcs can't draw circles)
  when (cur > 0 && cur < max) $ do
    -- Red segment
    S.path ! A.d (arcPath 0 (fromInteger cur / fromInteger max)) ! A.fill "#FF0000"
    -- Green segment
    S.path ! A.d (arcPath (fromInteger cur / fromInteger max) 1) ! A.fill "#05a727"
  -- Backup green circle
  when (cur == 0) $ S.circle ! A.cx "50" ! A.cy "50" ! A.r "50" ! A.fill "#05a727"
  -- Backup red circle
  when (cur == max) $ S.circle ! A.cx "50" ! A.cy "50" ! A.r "50" ! A.fill "#FF0000"
  -- White circle
  S.circle ! A.cx "50" ! A.cy "50" ! A.r "40" ! A.fill "#FFFFFF"
  -- Number overlay
  S.text_ ! A.y "60" !
            A.x "50" !
            A.style "text-anchor: middle; font-family: Droid Sans; font-size: 33;" $ fromString $ show (max - cur)

-- | Elliptical Arc path command
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

-- | Create an arc segment from Start to End around a circle
arcPath :: Double -- Start
        -> Double -- End
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

