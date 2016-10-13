module View exposing (..)


import Collage
import Element exposing (toHtml)
import Html exposing (Html)

import Entity exposing (drawBullet, drawPlayer, drawEnemy)
import Model exposing (Model)

-- VIEW

{-
drawEntity : Entity -> Collage.Form -> Collage.Form
drawEntity e s =
    s |> move ( e.x, e.y )


drawBullet : Bullet -> Collage.Form
drawBullet b =
    Collage.rect 10 10 |> filled Color.red |> (drawEntity b)


drawPlayer : Player -> Collage.Form
drawPlayer p =
    Collage.rect 40 40 |> filled Color.blue |> (drawEntity p)


drawEnemy : Enemy -> Collage.Form
drawEnemy e =
    Collage.rect 40 40 |> filled Color.red |> (drawEntity e)
-}


view : Model -> Html msg
view model =
    Collage.collage
        600
        600
        (List.concat
            [ [ drawPlayer model.player
              ]
            , List.map drawBullet model.bullets
            , List.map drawEnemy model.enemies
            ]
        )
        |> toHtml
