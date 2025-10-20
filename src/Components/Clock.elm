module Components.Clock exposing (eventHotTime, new, view, withEvents)

import Color
import Color.Convert
import Element as Ui exposing (Element)
import SizeRelations exposing (SizeRelation(..))
import String.Format
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


eventHotTime =
    5


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
        withinNextHour : Time.Zone -> Time.Posix -> Time.Posix -> Bool
        withinNextHour zone now target =
            let
                n =
                    Time.Extra.add Time.Extra.Millisecond -((eventHotTime * 60000) / 2 |> round) zone now |> Time.posixToMillis

                t =
                    target |> Time.posixToMillis

                td =
                    Time.Extra.add Time.Extra.Hour 1 settings.zone now
                        |> Time.posixToMillis
            in
            n < t && t < td
    in
    Settings { settings | events = List.filter (withinNextHour settings.zone settings.now) events }


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
        (circle
            [ cx radiusStr
            , cy radiusStr
            , r radiusStr
            , fill <| colorToHex <| settings.faceColor
            ]
            []
            :: ((case List.head settings.events of
                    Nothing ->
                        []

                    Just posix ->
                        if Time.Extra.compare posix settings.now == GT && Time.Extra.diff Time.Extra.Minute settings.zone settings.now posix < 30 then
                            [ viewEventArc radius
                                radius
                                radiusStr
                                ((minute + (second / 60)) / 60)
                                ((Time.toMinute settings.zone posix |> toFloat) / 60)
                                --TODO: Immer Arc rendern lassen, bei diff < 30 halt voll transparent?
                                (Time.Extra.diff Time.Extra.Minute settings.zone settings.now posix)
                            ]

                        else
                            []
                )
                    ++ [ viewQuarterLine settings.handColor quarterLineWidth radius 0.25
                       , viewQuarterLine settings.handColor quarterLineWidth radius 0.5
                       , viewQuarterLine settings.handColor quarterLineWidth radius 0.75
                       , viewQuarterLine settings.handColor quarterLineWidth radius 1
                       ]
                    ++ List.map (viewEvent radius settings.zone settings.now) settings.events
                    ++ [ viewHand settings.handColor handWidth (radius / 100 * 52) radius radiusStr ((hour + (minute / 60)) / 12)
                       , viewHand settings.handColor handWidth (radius / 100 * 77) radius radiusStr ((minute + (second / 60)) / 60)
                       ]
               )
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


viewEvent : Float -> Time.Zone -> Time.Posix -> Time.Posix -> Svg msg
viewEvent radius zone now eventPosix =
    let
        eventIsHot =
            (Time.Extra.diff Time.Extra.Millisecond zone now eventPosix |> toFloat) < ((eventHotTime * 60000) / 2)

        minutes =
            Time.toMinute zone eventPosix |> toFloat

        eventMinutes =
            minutes / 60

        angle turns =
            2 * pi * (turns - 0.25)

        segmentSize =
            -- Size of the event segment in minutes
            if eventIsHot then
                eventHotTime

            else
                2

        calcPos parts =
            let
                minute =
                    parts.minute |> toFloat

                second =
                    parts.second |> toFloat
            in
            (minute + (second / 60)) / 60

        eventMinus =
            Time.Extra.add Time.Extra.Second -((segmentSize * 60) // 2) zone eventPosix
                |> Time.Extra.posixToParts zone
                |> calcPos

        eventPlus =
            Time.Extra.add Time.Extra.Second ((segmentSize * 60) // 2) zone eventPosix
                |> Time.Extra.posixToParts zone
                |> calcPos
    in
    Svg.path
        [ d
            ("M {{x1}} {{y1}} L {{x2}} {{y2}} A {{radius}} {{radius}} 0 0 1 {{x3}} {{y3}} Z"
                |> String.Format.namedValue "x1"
                    (if eventIsHot then
                        radius |> String.fromFloat

                     else
                        radius + (radius / 100 * 80) * cos (angle eventMinutes) |> String.fromFloat
                    )
                |> String.Format.namedValue "y1"
                    (if eventIsHot then
                        radius |> String.fromFloat

                     else
                        radius + (radius / 100 * 80) * sin (angle eventMinutes) |> String.fromFloat
                    )
                |> String.Format.namedValue "x2" (radius + radius * cos (angle eventMinus) |> String.fromFloat)
                |> String.Format.namedValue "y2" (radius + radius * sin (angle eventMinus) |> String.fromFloat)
                |> String.Format.namedValue "radius" (String.fromFloat radius)
                |> String.Format.namedValue "x3" (radius + radius * cos (angle eventPlus) |> String.fromFloat)
                |> String.Format.namedValue "y3" (radius + radius * sin (angle eventPlus) |> String.fromFloat)
            )
        , stroke "#bb8800"
        , strokeWidth "0"
        , fill <|
            if eventIsHot then
                "#ff0000"

            else
                "#bb8800"
        ]
        []


viewEventArc : Float -> Float -> String -> Float -> Float -> Int -> Svg msg
viewEventArc length radius radiusStr thetaStart thetaEnd minuteDiff =
    let
        tStart =
            2 * pi * (thetaStart - 0.25)

        tEnd =
            2 * pi * (thetaEnd - 0.25)
    in
    Svg.path
        [ d
            ("M {{x1}} {{y1}} L {{x2}} {{y2}} A {{radius}} {{radius}} 0 0 1 {{x3}} {{y3}} Z"
                |> String.Format.namedValue "x1" radiusStr
                |> String.Format.namedValue "y1" radiusStr
                |> String.Format.namedValue "x2" (radius + length * cos tStart |> String.fromFloat)
                |> String.Format.namedValue "y2" (radius + length * sin tStart |> String.fromFloat)
                |> String.Format.namedValue "radius" (String.fromFloat length)
                |> String.Format.namedValue "x3" (radius + length * cos tEnd |> String.fromFloat)
                |> String.Format.namedValue "y3" (radius + length * sin tEnd |> String.fromFloat)
            )
        , stroke "#bb8800"
        , strokeWidth "0"
        , fill "#bb8800"
        , fillOpacity <| String.fromFloat <| abs ((minuteDiff |> toFloat) - 30) / 30 --"0.35"

        -- , strokeOpacity <| String.fromFloat <| abs ((minuteDiff |> toFloat) - 30) / 30
        ]
        []
