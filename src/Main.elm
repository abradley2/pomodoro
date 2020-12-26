port module Main exposing (main)

import Array exposing (Array)
import Audio exposing (Audio, AudioCmd(..))
import Colors exposing (..)
import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Events as Ev
import Element.Font as Font
import Element.Input as In
import Flags exposing (Flags)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import Mode exposing (Mode(..), ModeStatus(..))
import Onboarding
import Process
import String exposing (padLeft)
import Task
import Time exposing (Posix, millisToPosix, now, posixToMillis)
import UI


port audioPortToJS : E.Value -> Cmd msg


port audioPortFromJS : (D.Value -> msg) -> Sub msg


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


audio : Model -> Audio
audio model =
    case model.alarmSound of
        Loading ->
            Audio.silence

        Loaded _ Silenced ->
            Audio.silence

        Loaded source Paused ->
            Audio.silence

        Loaded source (Playing startTime) ->
            Audio.audio source startTime


main : Program Flags (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }


progressBar : Mode -> Posix -> El.Element Msg
progressBar mode currentTime =
    let
        width =
            300

        progressWidth =
            width * Mode.modeElapsed mode (Mode.modeDuration mode) currentTime

        progressWidthMin =
            if progressWidth < 16 then
                16

            else
                progressWidth
    in
    El.column
        []
        [ El.el
            [ Bg.color primaryLightColor
            , El.width (El.px width)
            , El.height (El.px 16)
            , Bd.rounded 20
            , El.inFront
                (El.el
                    [ Bg.color primaryColor
                    , El.width (El.px (round progressWidthMin))
                    , El.height (El.px 16)
                    , Bd.rounded 20
                    ]
                    (El.text "")
                )
            ]
            (El.text "")
        ]



-- INIT


type alias Model =
    { time : Posix
    , mode : Mode
    , modeStart : Maybe Posix
    , taskChecks : Array Bool
    , flags : Flags
    , alarmSound : AudioState
    , onboarding : Maybe Onboarding.Model
    }


type AudioState
    = Loading
    | Loaded Audio.Source LoadedAudioState


type LoadedAudioState
    = Silenced
    | Paused
    | Playing Time.Posix


prologueTimeView : Mode -> El.Element Msg
prologueTimeView mode =
    timeView mode (millisToPosix (Mode.modeDuration mode)) (millisToPosix (Mode.modeDuration mode))


displayTimeLeft : Posix -> Mode -> El.Element Msg
displayTimeLeft startPosix mode =
    case mode of
        Pomodoro (OnGoing val) ->
            timeView mode startPosix val

        Pomodoro Prologue ->
            prologueTimeView mode

        Pomodoro Epilogue ->
            El.el
                [ Font.color primaryColor
                , El.height (El.px 40)
                ]
                (El.el [ El.centerY ] (El.text "Enter checks for productivity"))

        ShortBreak (OnGoing val) ->
            timeView mode startPosix val

        ShortBreak Prologue ->
            prologueTimeView mode

        LongBreak (OnGoing val) ->
            timeView mode startPosix val

        LongBreak Prologue ->
            prologueTimeView mode

        _ ->
            El.text ""


timeView : Mode -> Posix -> Posix -> El.Element Msg
timeView mode currentPosix startPosix =
    Mode.modeDuration mode
        - (posixToMillis currentPosix - posixToMillis startPosix)
        |> timeText
        |> (\text ->
                El.column
                    []
                    [ El.el
                        [ Font.color primaryColor
                        , Font.semiBold
                        , Font.size 40
                        ]
                        (El.text text)
                    , El.html
                        (H.node "doc-title" [ A.attribute "data-value" text ] [])
                    ]
           )


timeText : Int -> String
timeText posixInt =
    posixInt
        |> (\v -> v // 1000)
        |> (\v -> ( v // 60, remainderBy 60 v ))
        |> Tuple.mapBoth
            (String.fromInt >> padLeft 2 '0')
            (String.fromInt >> padLeft 2 '0')
        |> (\( minutes, seconds ) ->
                minutes ++ ":" ++ seconds
           )


init : Flags -> ( Model, Cmd Msg, AudioCmd Msg )
init flags =
    ( { time = Time.millisToPosix 0
      , mode = Pomodoro Epilogue
      , modeStart = Nothing
      , taskChecks = Array.fromList [ False, False, False, False ]
      , flags = flags
      , alarmSound = Loading
      , onboarding = Nothing
      }
    , Task.perform
        GetTime
        now
    , Audio.loadAudio
        LoadedAlarmSound
        flags.alarmAudioUrl
    )



-- UPDATE


type Msg
    = GetTime Posix
    | SwitchMode Mode
    | RequestModeSwitch (Posix -> Mode)
    | CheckTask Int Bool
    | LoadedAlarmSound (Result Audio.LoadError Audio.Source)
    | OnboardingMsg Onboarding.Msg
    | ShowOnboarding


checkTimeUp : Posix -> Posix -> Mode -> Bool
checkTimeUp currentTime startTime mode =
    (posixToMillis currentTime - posixToMillis startTime)
        > Mode.modeDuration mode


checkTime : Mode -> Posix -> Maybe Mode
checkTime currentMode currentTime =
    case currentMode of
        Pomodoro (OnGoing startTime) ->
            if checkTimeUp currentTime startTime currentMode then
                Just (ShortBreak Prologue)

            else
                Nothing

        ShortBreak (OnGoing startTime) ->
            if checkTimeUp currentTime startTime currentMode then
                Just (Pomodoro Prologue)

            else
                Nothing

        LongBreak (OnGoing startTime) ->
            if checkTimeUp currentTime startTime currentMode then
                Just (Pomodoro Prologue)

            else
                Nothing

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update msg model =
    case msg of
        ShowOnboarding ->
            ( { model | onboarding = Just <| Onboarding.init model.flags }
            , Cmd.none
            , Audio.cmdNone
            )

        OnboardingMsg onboardingMsg ->
            let
                ( onboarding, mExtMsg ) =
                    Maybe.map
                        (Onboarding.update onboardingMsg >> Tuple.mapFirst Just)
                        model.onboarding
                        |> Maybe.withDefault ( Nothing, Nothing )

                newModel =
                    { model | onboarding = onboarding }
            in
            case mExtMsg of
                Just Onboarding.Close ->
                    ( { newModel | onboarding = Nothing }, Cmd.none, Audio.cmdNone )

                Nothing ->
                    ( newModel, Cmd.none, Audio.cmdNone )

        CheckTask taskNum isChecked ->
            ( { model
                | taskChecks =
                    model.taskChecks
                        |> Array.set taskNum isChecked
              }
            , Cmd.none
            , Audio.cmdNone
            )

        GetTime newTime ->
            ( { model
                | time = newTime
                , mode =
                    checkTime model.mode newTime
                        |> Maybe.withDefault model.mode
                , alarmSound =
                    case ( checkTime model.mode newTime, model.alarmSound ) of
                        ( Just _, Loaded source _ ) ->
                            Loaded source (Playing newTime)

                        _ ->
                            model.alarmSound
              }
            , Process.sleep 100
                |> Task.andThen (always Time.now)
                |> Task.perform GetTime
            , Audio.cmdNone
            )

        SwitchMode newMode ->
            ( { model
                | mode = newMode
                , taskChecks =
                    case newMode of
                        LongBreak _ ->
                            Array.fromList [ False, False, False, False ]

                        _ ->
                            model.taskChecks
              }
            , Cmd.none
            , Audio.cmdNone
            )

        RequestModeSwitch timeToMode ->
            ( { model | modeStart = Just model.time }
            , Task.perform
                (timeToMode >> SwitchMode)
                Time.now
            , Audio.cmdNone
            )

        LoadedAlarmSound res ->
            case res of
                Result.Ok source ->
                    ( { model | alarmSound = Loaded source Paused }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                _ ->
                    ( model, Cmd.none, Audio.cmdNone )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


checkCount : Array Bool -> Int
checkCount =
    Array.toList
        >> List.filter ((==) True)
        >> List.length


onboardingModal : Onboarding.Model -> El.Element Msg
onboardingModal onboarding =
    El.el
        [ Bg.color (El.rgba255 0 0 0 0.4)
        , El.height El.fill
        , El.width El.fill
        , El.paddingEach { edges | top = 100 }
        ]
        (El.column
            [ El.width (El.px 320)
            , El.height (El.px 320)
            , Bg.color white
            , Bd.rounded 10
            , El.centerX
            , El.padding 10
            ]
            [ El.el [] (El.text "Im a modal") ]
        )



-- VIEW


view : Model -> H.Html Msg
view model =
    El.layout
        [ El.inFront
            (case model.onboarding of
                Just onboarding ->
                    onboardingModal onboarding

                Nothing ->
                    El.el
                        [ El.paddingXY 24 24
                        , El.alignTop
                        , El.alignRight
                        ]
                        (In.button
                            (UI.activeButton ++
                                [ Bd.rounded 9999
                                , El.paddingXY 4 4
                                , El.width (El.px 90)
                                ]
                            )
                            { label = El.row [ El.centerX ] 
                                [ El.html <|
                                    H.i  [ A.class "fa fa-info-circle", A.style "margin-right" "12px" ] []
                                , El.text "Info"
                                ]
                            , onPress = Just ShowOnboarding
                            }
                        )
            )
        ]
    <|
        El.column
            [ El.centerY
            , El.width El.fill
            ]
            [ El.el
                [ El.centerX
                ]
                (displayTimeLeft model.time model.mode)
            , El.row
                [ El.centerX
                , El.spacing 20
                , El.height (El.px 100)
                ]
                (case model.mode of
                    Pomodoro Epilogue ->
                        [ UI.checkbox (CheckTask 0) (model.taskChecks |> Array.get 0 |> Maybe.withDefault False)
                        , UI.checkbox (CheckTask 1) (model.taskChecks |> Array.get 1 |> Maybe.withDefault False)
                        , UI.checkbox (CheckTask 2) (model.taskChecks |> Array.get 2 |> Maybe.withDefault False)
                        , UI.checkbox (CheckTask 3) (model.taskChecks |> Array.get 3 |> Maybe.withDefault False)
                        , In.button
                            []
                            { label =
                                El.row
                                    []
                                    [ El.el
                                        [ Font.color primaryColor
                                        , Font.size 16
                                        ]
                                        (El.text "Next ")
                                    , El.html (H.i [ A.class "fa fa-play", A.style "color" "rgb(0 209 178)" ] [])
                                    ]
                            , onPress =
                                Just
                                    (SwitchMode
                                        (if checkCount model.taskChecks == 4 then
                                            LongBreak (OnGoing model.time)

                                         else
                                            ShortBreak (OnGoing model.time)
                                        )
                                    )
                            }
                        ]

                    _ ->
                        []
                )
            , El.el
                [ El.centerX
                , El.centerY
                , El.height (El.px 70)
                ]
                (progressBar model.mode model.time)
            , El.wrappedRow
                [ El.spacing 20
                , El.centerX
                , El.centerY
                ]
                [ In.button
                    (case model.mode of
                        Pomodoro _ ->
                            UI.activeButton

                        _ ->
                            UI.passiveButton
                                (Ev.onClick <|
                                    RequestModeSwitch (OnGoing >> Pomodoro)
                                )
                    )
                    { onPress = Nothing, label = El.text "Pomodoro" }
                , In.button
                    (case model.mode of
                        ShortBreak _ ->
                            UI.activeButton

                        _ ->
                            UI.passiveButton
                                (Ev.onClick <|
                                    RequestModeSwitch (OnGoing >> ShortBreak)
                                )
                    )
                    { onPress = Nothing, label = El.text "Short Break" }
                , In.button
                    (case model.mode of
                        LongBreak _ ->
                            UI.activeButton

                        _ ->
                            UI.passiveButton
                                (Ev.onClick <|
                                    RequestModeSwitch (OnGoing >> LongBreak)
                                )
                    )
                    { onPress = Nothing, label = El.text "Long Break" }
                ]
            , El.row
                [ El.height (El.px 200)
                , El.width El.fill
                ]
                []
            ]
