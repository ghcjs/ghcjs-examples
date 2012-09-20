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

-- | This tree structure is used to give a list of behaviours from different
-- parts of the FRP logic, so the engine can efficiently draw only the things
-- that have changed. This could be done just as a plain old list, this is a
-- bit more flexible, and also, it allows for us to later add the ability to
-- switch subtrees by means of Sodium's switch primitive.
data BehaviorTree a = BehaviorTree a :++ BehaviorTree a
                    | BehaviorNode (Behavior a)
infixr 5 :++

