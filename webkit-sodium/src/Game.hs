module Game where

import FRP.Sodium


type Coord = Double
type Point = (Coord, Coord)
type Vector = (Coord, Coord)
type Rect = (Point, Vector)   -- Central point and size from centre to edge
type Sprite = (Rect, String)

data MouseEvent = MouseDown Point | MouseMove Point | MouseUp Point
    deriving Show

plus :: Point -> Vector -> Point
plus (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

minus :: Point -> Point -> Vector
minus (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

-- | True if the point is inside the rectangle
inside :: Point -> Rect -> Bool
inside (x, y) ((ox, oy), (wx, wy)) =
    x >= ox - wx && x <= ox + wx &&
    y >= oy - wy && y <= oy + wy

infixr 5 :++
data BehaviorTree a = BehaviorTree a :++ BehaviorTree a
                    | BehaviorNode (Behavior a)

