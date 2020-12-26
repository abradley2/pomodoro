module Mode exposing (..)

import Time exposing (Posix, millisToPosix, posixToMillis)


type ModeStatus
    = Prologue
    | OnGoing Posix
    | Epilogue


type Mode
    = Pomodoro ModeStatus
    | ShortBreak ModeStatus
    | LongBreak ModeStatus


modeDuration : Mode -> Int
modeDuration mode =
    case mode of
        Pomodoro _ ->
            1000 * 60 * 25 + 900

        ShortBreak _ ->
            1000 * 60 * 1 + 900

        LongBreak _ ->
            1000 * 60 * 10 + 900


modeDurationPosix : Mode -> Posix
modeDurationPosix =
    modeDuration >> millisToPosix


modeElapsed : Mode -> Int -> Posix -> Float
modeElapsed mode duration currentTime =
    case mode of
        Pomodoro Prologue ->
            0.0

        Pomodoro (OnGoing startTime) ->
            let
                start =
                    posixToMillis startTime |> toFloat

                current =
                    posixToMillis currentTime |> toFloat
            in
            (current - start) / toFloat duration

        Pomodoro Epilogue ->
            1.0

        ShortBreak m ->
            modeElapsed (Pomodoro m) duration currentTime

        LongBreak m ->
            modeElapsed (Pomodoro m) duration currentTime
