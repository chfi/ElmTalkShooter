module Model exposing (Model, update, init, subscriptions)

import Random
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)
import AnimationFrame

import Entity exposing (Player(..), Enemy(..), Bullet(..))
import Input exposing (..)


-- TODO: this should also contain current window size, or something like that
type alias Model =
    { player : Player
    , enemies : List Enemy
    , bullets : List Bullet
    , keys : KeyState
    }

playerSpeed = 4


-- init : Model, Cmd
init = ({ player = Entity.initPlayer, enemies = [], bullets = [], keys = initKeys }, Cmd.none)


-- MESSAGES



type Msg
    = KeyMsg KeyDir KeyInput
    | TimeTick Time
    | FrameTick Float
    | SpawnEnemy Float
    | Shoot
    | NoOp


type alias KeyState =
    { up : Bool, down : Bool, left : Bool, right : Bool }


initKeys : KeyState
initKeys =
    { up = False, down = False, left = False, right = False }



-- UPDATE

enemyGenerator : Cmd Msg
enemyGenerator = Random.generate SpawnEnemy (Random.float 0 300)
    -- let x = (Random.float 0 1000)
    -- in
    --     Random.generate SpawnEnemy x


spawnEnemy : Float -> Enemy
spawnEnemy x = Entity.createEnemy x 200 0 (-2)

shoot : Player -> Bullet
shoot (Player p _) = Entity.createBullet p.rect.x p.rect.y 0 5


updateKeyState : KeyDir -> KeyInput -> KeyState -> KeyState
updateKeyState dir input keys =
    let
        dirToBool dir =
            case dir of
                KeyUp ->
                    False

                KeyDown ->
                    True
    in
        case input of
            Left ->
                { keys | left = dirToBool dir }

            Right ->
                { keys | right = dirToBool dir }

            Up ->
                { keys | up = dirToBool dir }

            Down ->
                { keys | down = dirToBool dir }

            _ -> keys


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg dir input ->
            let
                keys' =
                    updateKeyState dir input model.keys

                model' =
                    { model | keys = keys' }
                        |> setPlayerVel
            in
                ( model', Cmd.none )

        FrameTick dt ->
            ( step dt model, Cmd.none )

        TimeTick t ->
            ( model, Cmd.none )

        SpawnEnemy x ->
            ( { model | enemies = spawnEnemy x :: model.enemies }, Cmd.none )

        Shoot ->
            ( { model | bullets = shoot model.player :: model.bullets }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


step : Float -> Model -> Model
step dt model =
    let
        p' =
            Entity.updatePlayer dt model.player

        bs' =
            List.map (Entity.updateBullet dt) model.bullets
            -- List.map (Entity.updateEntity dt) model.bullets
            -- |> List.filterMap (killBullets model.enemies)

        es' =
            List.map (Entity.updateEnemy dt) model.enemies
            -- |> List.filterMap (Entity.collideBullets model.bullets)

    in
        { model | player = p', enemies = es', bullets = bs' }


-- killBullets : List Enemy -> Bullet -> Maybe Bullet
-- killBullets es b =
--     let (Bullet bulletEntity) = b
--         enemies = List.map (\ (Enemy e) -> e) es
--     in
--     Entity.collideManyWith (\b' -> Bullet b') bulletEntity enemies




setPlayerVel : Model -> Model
setPlayerVel model =
    let
        keys =
            model.keys

        (Player pEntity p) =
            model.player

        vx =
            if keys.left then
                -p.speed
            else if keys.right then
                p.speed
            else
                0

        vy =
            if keys.up then
                p.speed
            else if keys.down then
                -p.speed
            else
                0

        pEntity' =
            { pEntity | vx = vx, vy = vy }
    in
        { model | player = Player pEntity' p }



cullEnemies : List (Maybe Enemy) -> List Enemy
cullEnemies mes = List.foldr (\e es -> case e of
                                           Just e -> e :: es
                                           Nothing -> es) [] mes


-- updateEntity : Float -> Entity -> Entity
-- updateEntity dt entity =
--     { entity | x = entity.x + entity.vx, y = entity.y + entity.vy }


-- updatePlayer : Float -> Player -> Player
-- updatePlayer =
--     updateEntity

-- updateEnemy : Float -> Enemy -> Enemy
-- updateEnemy =
--     updateEntity


-- updateBullet : Float -> Bullet -> Bullet
-- updateBullet = updateEntity


-- SUBSCRIPTIONS


keyCodeToMsg : KeyDir -> KeyCode -> Msg
keyCodeToMsg dir kc =
    case keyCodeToInput kc of
        Nothing ->
            NoOp

        Just ShootKey ->
            Shoot

        Just key ->
            KeyMsg dir key


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (keyCodeToMsg KeyDown)
        , Keyboard.ups (keyCodeToMsg KeyUp)
        , AnimationFrame.diffs FrameTick
        , Time.every second (\_ -> NoOp)
        ]
