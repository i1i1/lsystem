--
-- Draw L system to svg
--

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Draw (Settings(..), draw, drawItem, foldDraw, emptyElem) where

import           L

import           Data.Text    (Text, pack)
import           GHC.Float    (double2Int)
import           Graphics.Svg (AttrTag (..), Element, ToElement, doctype,
                               svg11_, toElement, with, (<<-))
import qualified Graphics.Svg as S


data Settings = Settings { start       :: (Double, Double)
                         , size        :: (Double, Double)
                         , by_angle    :: Double
                         , line_length :: Double
                         , line_width  :: Double
                         , line_mult   :: Double
                         } deriving (Show, Eq)

data State = State { coor       :: (Double, Double)
                   , stack      :: [(Double, Double, Double, Double)]
                   , angle      :: Double
                   , line_mult_ :: Double
                   } deriving (Show, Eq)

svgLine :: Double -> (Double, Double) -> (Double, Double) -> Element
svgLine width (x1, y1) (x2, y2) =
    S.line_ [ S.X1_ <<- pack (show x1)
            , S.Y1_ <<- pack (show y1)
            , S.X2_ <<- pack (show x2)
            , S.Y2_ <<- pack (show y2)
            , S.Fill_ <<- "black"
            , S.Stroke_ <<- "black"
            , S.Stroke_linecap_ <<- "round"
            , S.Stroke_width_ <<- pack (show width)
            ]

drawVariable :: Settings -> State -> Variable -> (Element, State)
drawVariable _ state (Char _) = (emptyElem, state)
drawVariable Settings { .. } State { coor = (x, y), .. } F =
    let
      new_x = x + (sin angle) * line_mult * line_length
      new_y = y + (cos angle) * line_mult * line_length
    in
      (svgLine line_width (x, y) (new_x, new_y), State { coor = (new_x, new_y), .. })

drawVariable Settings { .. } State { coor = (x, y), .. } Fd =
    let
      new_x = x + (sin angle) * line_mult * line_length
      new_y = y + (cos angle) * line_mult * line_length
    in
      (emptyElem, State { coor = (new_x, new_y), .. })

drawItem :: Settings -> State -> Item -> (Element, State)
drawItem Settings { .. } state Reverse =
    (emptyElem, state { angle = (angle state) - pi })
drawItem Settings { .. } state RotL =
    (emptyElem, state { angle = (angle state) - by_angle })
drawItem Settings { .. } state RotR =
    (emptyElem, state { angle = (angle state) + by_angle })

drawItem Settings { .. } state IncL =
    (emptyElem, state { line_mult_ = (line_mult_ state) * line_mult })
drawItem Settings { .. } state DecL =
    (emptyElem, state { line_mult_ = (line_mult_ state) / line_mult })

drawItem _ State { coor = (x, y), .. } Push =
    (emptyElem, State { coor = (x, y), stack = ((x, y, angle, line_mult_):stack), .. })
drawItem
    _
    State { stack = ((new_x, new_y, new_angle, new_line):stack), .. }
    Pop =
        (emptyElem, State { coor = (new_x, new_y), angle = new_angle, stack, line_mult_ = new_line })
drawItem settings state (Variable var) = drawVariable settings state var

svg :: (Double, Double) -> Element -> Element
svg (x, y) content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1"
                           , Width_  <<- (pack (show (double2Int x)))
                           , Height_ <<- (pack (show (double2Int y)))
                           ]


emptyElem :: Element
emptyElem = toElement ("" :: Text)

foldDraw :: Settings -> (State, Element) -> Item -> (State, Element)
foldDraw settings (s, e) i = let (ne, ns) = drawItem settings s i in (ns, e <> ne)

draw :: Settings -> [Item] -> Element
draw Settings { .. } l =
    let
      state = State { coor = start
                    , angle = 0
                    , stack = []
                    , line_mult_ = 1
                    }
      (_, e) = foldl (foldDraw Settings { .. }) (state, emptyElem) l
    in svg size e
