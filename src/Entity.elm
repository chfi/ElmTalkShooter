-- module Entity exposing (Player(..), Enemy(..), Bullet(..))
module Entity exposing (..)

import Collage exposing (filled, move)
import Color
import Maybe exposing (andThen)
import Geometry exposing (Rect, overlapRects)


type alias Entity =
    { rect : Rect, vx : Float, vy : Float }

-- TODO: add firing timeout that's decreased by dt each step


type Player
    = Player Entity { speed : Float }


type Enemy
    = Enemy Entity


type Bullet
    = Bullet Entity


initPlayer : Player
initPlayer =
    Player
        { rect = { x = 0, y = 0, w = 30, h = 30 }
        , vx = 0
        , vy = 0
        }
        { speed = 4 }


createEntity : Float -> Float -> Float -> Float -> Entity
createEntity x y vx vy = { rect = { x=x, y=y, w = 30, h = 30 }, vx=vx, vy=vy }

createEnemy : Float -> Float -> Float -> Float -> Enemy
createEnemy x y vx vy = createEntity x y vx vy |> Enemy

createBullet : Float -> Float -> Float -> Float -> Bullet
createBullet x y vx vy = createEntity x y vx vy |> Bullet


drawEntity : Entity -> (Collage.Shape -> Collage.Form) -> Collage.Form
drawEntity { rect } s =
    Collage.rect rect.w rect.h |> s |> move ( rect.x, rect.y )


drawBullet : Bullet -> Collage.Form
drawBullet (Bullet b) =
    filled Color.black |> drawEntity b


drawPlayer : Player -> Collage.Form
drawPlayer (Player p _) =
    filled Color.blue |> drawEntity p


drawEnemy : Enemy -> Collage.Form
drawEnemy (Enemy e) =
    filled Color.red |> drawEntity e



-- UPDATE


collidePair : Entity -> Entity -> Bool
collidePair e1 e2 =
    overlapRects e1.rect e2.rect



collideMany : Entity -> List Entity -> Maybe Entity
collideMany e es =
    if List.any (collidePair e) es then
        Nothing
    else
        Just e


-- checks if `e` collides with each element in `es`,
-- and applies `f` to all those elements that _did not_ collide with `e`.
-- useful for removing collided enemies then retagging with the Enemy type
collideManyWith : (Entity -> a) -> Entity -> List Entity -> List a
collideManyWith f e es =
    List.filter (\e' -> collidePair e e') es
        |> List.map f

collideBullets : Enemy -> List Bullet -> Maybe Enemy
collideBullets (Enemy e) bs =
    (List.map (\ (Bullet b) -> b) bs |> collideMany e)
        `andThen` (\x -> Just (Enemy x))


updateEntity : Float -> Entity -> Entity
updateEntity dt entity =
    let entityRect = entity.rect
        newX = entity.rect.x + entity.vx
        newY = entity.rect.y + entity.vy
        newRect = { entityRect | x = newX, y = newY }
    in
        {entity | rect = newRect }


updatePlayer : Float -> Player -> Player
updatePlayer dt (Player entity p) =
    let newEntity = updateEntity dt entity
    in
    Player newEntity p


updateEnemy : Float -> Enemy -> Enemy
updateEnemy dt (Enemy entity) = updateEntity dt entity |> Enemy

updateBullet : Float -> Bullet -> Bullet
updateBullet dt (Bullet entity) = updateEntity dt entity |> Bullet

-- updateEnemy dt (Enemy entity) =
--     Enemy (updateEntity dt entity)
