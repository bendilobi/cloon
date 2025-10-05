module Components.Clock exposing (new, view)

import Color
import Color.Convert
import Element as Ui exposing (Element)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type Clock
    = Settings
        { size : Float
        , zone : Time.Zone
        , now : Time.Posix
        , background : Ui.Color
        , foreground : Ui.Color
        }


new :
    { size : Float
    , zone : Time.Zone
    , now : Time.Posix
    , background : Ui.Color
    , foreground : Ui.Color
    }
    -> Clock
new props =
    Settings
        { size = props.size
        , zone = props.zone
        , now = props.now
        , background = props.background
        , foreground = props.foreground
        }


colorToHex : Ui.Color -> String
colorToHex color =
    color
        |> Ui.toRgb
        |> Color.fromRgba
        |> Color.Convert.colorToHex


view : Clock -> Element msg
view (Settings settings) =
    let
        hour =
            toFloat (Time.toHour settings.zone settings.now)

        minute =
            toFloat (Time.toMinute settings.zone settings.now)

        second =
            toFloat (Time.toSecond settings.zone settings.now)

        sizeStr =
            String.fromFloat settings.size

        radius =
            settings.size / 2

        radiusStr =
            settings.size / 2 |> String.fromFloat

        handColor =
            settings.background

        handWidth =
            settings.size * 0.033

        quarterLineWidth =
            settings.size * 0.01
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
            , fill <| colorToHex <| settings.foreground
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
