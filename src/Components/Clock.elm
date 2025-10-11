module Components.Clock exposing (new, view, withEvents)

import Color
import Color.Convert
import Element as Ui exposing (Element)
import SizeRelations exposing (SizeRelation(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Time.Extra


type Clock
    = Settings
        { size : Float
        , zone : Time.Zone
        , now : Time.Posix
        , handColor : Ui.Color
        , faceColor : Ui.Color
        , events : List Time.Posix
        }


new :
    { size : Float
    , zone : Time.Zone
    , now : Time.Posix
    , handColor : Ui.Color
    , faceColor : Ui.Color
    }
    -> Clock
new props =
    Settings
        { size = props.size
        , zone = props.zone
        , now = props.now
        , handColor = props.handColor
        , faceColor = props.faceColor
        , events = []
        }


withEvents : List Time.Posix -> Clock -> Clock
withEvents events (Settings settings) =
    let
        withinNextHour : Time.Posix -> Time.Posix -> Bool
        withinNextHour now target =
            let
                n =
                    now |> Time.posixToMillis

                t =
                    target |> Time.posixToMillis

                td =
                    Time.Extra.add Time.Extra.Hour 1 settings.zone now
                        |> Time.posixToMillis
            in
            n < t && t < td
    in
    Settings { settings | events = List.filter (withinNextHour settings.now) events }


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
            radius |> String.fromFloat

        relSize =
            SizeRelations.size settings.size

        handWidth =
            relSize HandWidth

        quarterLineWidth =
            relSize QuarterLineWidth
    in
    svg
        [ viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
        , width sizeStr
        , height sizeStr
        ]
        ([ circle
            [ cx radiusStr
            , cy radiusStr
            , r radiusStr
            , fill <| colorToHex <| settings.faceColor
            ]
            []
         , viewQuarterLine settings.handColor quarterLineWidth radius 0.25
         , viewQuarterLine settings.handColor quarterLineWidth radius 0.5
         , viewQuarterLine settings.handColor quarterLineWidth radius 0.75
         , viewQuarterLine settings.handColor quarterLineWidth radius 1
         ]
            ++ List.map (viewEvent radius (relSize EventLineWidth) settings.zone) settings.events
            ++ [ viewHand settings.handColor handWidth (radius / 100 * 52) radius radiusStr ((hour + (minute / 60)) / 12)
               , viewHand settings.handColor handWidth (radius / 100 * 77) radius radiusStr ((minute + (second / 60)) / 60)
               ]
        )
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


viewEvent : Float -> Float -> Time.Zone -> Time.Posix -> Svg msg
viewEvent radius width zone eventPosix =
    let
        turns =
            (Time.toMinute zone eventPosix |> toFloat) / 60

        t =
            2 * pi * (turns - 0.25)

        x_1 =
            radius + (radius / 100 * 75) * cos t |> String.fromFloat

        y_1 =
            radius + (radius / 100 * 75) * sin t |> String.fromFloat

        x_2 =
            radius + radius * cos t |> String.fromFloat

        y_2 =
            radius + radius * sin t |> String.fromFloat
    in
    line
        [ x1 x_1
        , y1 y_1
        , x2 x_2
        , y2 y_2
        , stroke "#bb8800"
        , strokeWidth (String.fromFloat width)

        -- , strokeLinecap "round"
        ]
        []
