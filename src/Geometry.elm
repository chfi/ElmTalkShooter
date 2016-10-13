module Geometry exposing (Rect, overlapRects)


type alias Rect =
    { x : Float, y : Float, w : Float, h : Float }


overlapRects : Rect -> Rect -> Bool
overlapRects r1 r2 =
    (r1.x - r1.w / 2)
        < (r2.x + r2.w / 2)
        && (r1.x + r1.w / 2)
        > (r2.x - r2.w / 2)
        && (r1.y - r1.h / 2)
        < (r2.y + r2.h / 2)
        && (r1.y + r1.h / 2)
        > (r2.y - r2.h / 2)
