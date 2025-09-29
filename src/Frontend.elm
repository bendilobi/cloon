module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Color
import Color.Convert
import Element as Ui
import Element.Background as Bg
import Element.Font as Font exposing (color)
import Element.Input as Input
import Html exposing (Html)
import Lamdera exposing (sendToBackend)
import Platform.Cmd as Cmd
import String.Format
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Cloon"
                , body = [ view model ]
                }
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , size = 200
      , dateHidden = True
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        , Task.perform ViewportReceived Browser.Dom.getViewport
        ]
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ViewportReceived { viewport } ->
            ( { model | size = Basics.min viewport.width viewport.height }
            , Cmd.none
            )

        Resized width height ->
            ( { model | size = Basics.min width height |> toFloat }
            , Cmd.none
            )

        DateToggled ->
            let
                newHidden =
                    not model.dateHidden
            in
            ( { model | dateHidden = newHidden }
            , sendToBackend <| DateHiddenChanged newHidden
            )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize Resized
        ]


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewDateHidden isHidden ->
            ( { model | dateHidden = isHidden }
            , Cmd.none
            )


colors : { foreground : Ui.Color, background : Ui.Color }
colors =
    { foreground = Ui.rgb255 241 241 230
    , background = Ui.rgb255 21 35 65
    }


colorToHex : Ui.Color -> String
colorToHex color =
    color
        |> Ui.toRgb
        |> Color.fromRgba
        |> Color.Convert.colorToHex


view : Model -> Html FrontendMsg
view model =
    Ui.layoutWith
        { options =
            [ Ui.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
    <|
        Ui.el
            [ Ui.width Ui.fill
            , Ui.height Ui.fill
            , Bg.color colors.background
            ]
        <|
            Ui.column
                [ Ui.width Ui.fill
                , Ui.spacing (model.size * 0.08 |> round)
                , Ui.centerY
                ]
                [ Input.button
                    [ Ui.centerX ]
                    { label =
                        Ui.el [] <|
                            viewClock model (model.size * 0.6)
                    , onPress = Just DateToggled
                    }
                , Ui.el
                    [ Ui.transparent model.dateHidden
                    , Font.center
                    , Font.color <| colors.foreground
                    , Font.size <| (model.size * 0.07 |> round)
                    , Font.family
                        [ Font.external
                            { name = "Pompiere"
                            , url = "https://fonts.googleapis.com/css2?family=Pompiere"
                            }

                        --   Font.external
                        --     { name = "National Park"
                        --     , url = "https://fonts.googleapis.com/css2?family=National+Park:wght@200..800"
                        --     }
                        , Font.typeface "Verdana"
                        , Font.sansSerif
                        ]
                    , Ui.centerX
                    ]
                  <|
                    Ui.text <|
                        viewDate model
                ]


viewClock : Model -> Float -> Ui.Element msg
viewClock model size =
    let
        hour =
            toFloat (Time.toHour model.zone model.time)

        minute =
            toFloat (Time.toMinute model.zone model.time)

        second =
            toFloat (Time.toSecond model.zone model.time)

        sizeStr =
            String.fromFloat size

        radius =
            size / 2

        radiusStr =
            size / 2 |> String.fromFloat

        handColor =
            colors.background

        handWidth =
            size * 0.033

        quarterLineWidth =
            size * 0.01
    in
    svg
        [ viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
        , width sizeStr
        , height sizeStr
        ]
        [ circle
            [ cx radiusStr
            , cy radiusStr
            , r radiusStr
            , fill <| colorToHex <| colors.foreground
            ]
            []
        , viewQuarterLine handColor quarterLineWidth radius 0.25
        , viewQuarterLine handColor quarterLineWidth radius 0.5
        , viewQuarterLine handColor quarterLineWidth radius 0.75
        , viewQuarterLine handColor quarterLineWidth radius 1
        , viewHand handColor handWidth (radius / 100 * 52) radius radiusStr ((hour + (minute / 60)) / 12)
        , viewHand handColor handWidth (radius / 100 * 77) radius radiusStr ((minute + (second / 60)) / 60)
        ]
        |> Ui.html


viewQuarterLine : Ui.Color -> Float -> Float -> Float -> Svg.Svg msg
viewQuarterLine color width radius turns =
    let
        t =
            2 * pi * turns

        x_1 =
            radius + (radius / 100 * 85) * cos t |> String.fromFloat

        y_1 =
            radius + (radius / 100 * 85) * sin t |> String.fromFloat

        x_2 =
            radius + (radius / 100 * 95) * cos t |> String.fromFloat

        y_2 =
            radius + (radius / 100 * 95) * sin t |> String.fromFloat
    in
    line
        [ x1 x_1
        , y1 y_1
        , x2 x_2
        , y2 y_2
        , stroke <| colorToHex color
        , strokeWidth (String.fromFloat width)
        , strokeLinecap "round"
        ]
        []


viewHand : Ui.Color -> Float -> Float -> Float -> String -> Float -> Svg.Svg msg
viewHand color width length radius radiusStr turns =
    let
        t =
            2 * pi * (turns - 0.25)

        x =
            radius + length * cos t

        y =
            radius + length * sin t
    in
    line
        [ x1 radiusStr
        , y1 radiusStr
        , x2 (String.fromFloat x)
        , y2 (String.fromFloat y)
        , stroke <| colorToHex color
        , strokeWidth (String.fromFloat width)
        , strokeLinecap "round"
        ]
        []


viewDate : Model -> String
viewDate model =
    "{{day}}. {{month}} {{year}}"
        |> String.Format.namedValue "day"
            (Time.toDay model.zone model.time |> String.fromInt)
        |> String.Format.namedValue "month"
            (Time.toMonth model.zone model.time |> toGermanMonth)
        |> String.Format.namedValue "year"
            (Time.toYear model.zone model.time |> String.fromInt)


toGermanMonth : Time.Month -> String
toGermanMonth month =
    case month of
        Time.Jan ->
            "Januar"

        Time.Feb ->
            "Februar"

        Time.Mar ->
            "März"

        Time.Apr ->
            "April"

        Time.May ->
            "Mai"

        Time.Jun ->
            "Juni"

        Time.Jul ->
            "Juli"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "Oktober"

        Time.Nov ->
            "November"

        Time.Dec ->
            "Dezember"
