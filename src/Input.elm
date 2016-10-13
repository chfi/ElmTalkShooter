module Input exposing (..)

import Keyboard exposing (KeyCode)


type KeyInput
    = Up
    | Down
    | Left
    | Right
    | ShootKey

type KeyDir
    = KeyUp
    | KeyDown

type alias KeyState =
    { up : Bool, down : Bool, left : Bool, right : Bool }


initKeys : KeyState
initKeys =
    { up = False, down = False, left = False, right = False }


keyCodeToInput : KeyCode -> Maybe KeyInput
keyCodeToInput code =
    case code of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

                -- shoot on spacebar
        32 ->
            Just ShootKey

        _ ->
            Nothing





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
