module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App
import AnimationFrame
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Time exposing (Time, second)

import Model
import View
import Entity


-- MAIN

main : Program Never
main =
    Html.App.program
        { init = Model.init
        , view = View.view
        , update = Model.update
        , subscriptions = Model.subscriptions
        }
