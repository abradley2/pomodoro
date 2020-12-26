module Onboarding exposing (..)

import Array exposing (Array)
import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Flags exposing (Flags)


type Msg
    = Next
    | Previous


type ExtMsg
    = Close


type alias Model =
    { index : Int
    , slides : Array String
    }


init : Flags -> Model
init flags =
    { index = 0
    , slides =
        Array.fromList
            [ flags.pomodoroImageUrl
            , flags.epilogueImageUrl
            , flags.breakImageUrl
            ]
    }


update : Msg -> Model -> ( Model, Maybe ExtMsg )
update msg model =
    ( model, Nothing )


view : El.Element Msg
view =
    El.el [] (El.text "test")
