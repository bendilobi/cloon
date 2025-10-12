module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Components.Clock as Clock
import Dict
import Element exposing (..)
import Element.Background as Bg
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (focusedOnLoad)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Lamdera exposing (sendToBackend)
import Platform.Cmd as Cmd
import SizeRelations as Rel exposing (SizeRelation(..))
import String exposing (toInt)
import String.Format
import Task
import Time
import Time.Extra
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
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , size = 200

      --   , relSize = Rel.size 200
      , dateHidden = True
      , mouseOver = False
      , schedule = Dict.empty
      , scheduleShown = False
      , currentHourInput = "00"
      , currentMinutesInput = "00"
      , currentDescInput = ""
      , currentPoolname = ""
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
            -- ( { model | relSize = Rel.size <| Basics.min viewport.width viewport.height }
            ( { model | size = Basics.min viewport.width viewport.height }
            , Cmd.none
            )

        Resized width height ->
            -- ( { model | relSize = Rel.size <| toFloat <| Basics.min width height }
            ( { model | size = toFloat <| Basics.min width height }
            , Cmd.none
            )

        DateToggled ->
            let
                newHidden =
                    not model.dateHidden
            in
            ( { model | dateHidden = newHidden }
            , Cmd.none
            )

        MouseOver over ->
            ( { model | mouseOver = over }
            , Cmd.none
            )

        ScheduleToggled ->
            ( { model | scheduleShown = not model.scheduleShown }
            , Cmd.none
            )

        HourInputChanged newHour ->
            ( { model
                | currentHourInput =
                    case toInt newHour of
                        Nothing ->
                            if newHour == "" then
                                ""

                            else
                                model.currentHourInput

                        Just t ->
                            if t >= 0 && t < 24 && String.length newHour < 3 then
                                newHour

                            else
                                model.currentHourInput
              }
            , Cmd.none
            )

        MinutesInputChanged newMinutes ->
            ( { model
                | currentMinutesInput =
                    case toInt newMinutes of
                        Nothing ->
                            if newMinutes == "" then
                                ""

                            else
                                model.currentMinutesInput

                        Just t ->
                            if t >= 0 && t < 60 && String.length newMinutes < 3 then
                                newMinutes

                            else
                                model.currentMinutesInput
              }
            , Cmd.none
            )

        DescInputChanged newDesc ->
            ( { model
                | currentDescInput =
                    if String.length newDesc > 30 then
                        model.currentDescInput

                    else
                        newDesc
              }
            , Cmd.none
            )

        AddEventPressed ->
            let
                hours =
                    model.currentHourInput |> toInt |> Maybe.withDefault 0

                minutes =
                    model.currentMinutesInput |> toInt |> Maybe.withDefault 0

                currentTimeParts =
                    Time.Extra.posixToParts model.zone model.time

                eventMillis =
                    Time.Extra.partsToPosix model.zone
                        { currentTimeParts
                            | hour = hours
                            , minute = minutes
                            , second = 0
                            , millisecond = 0
                        }
                        |> (if hours < currentTimeParts.hour then
                                Time.Extra.add Time.Extra.Day 1 model.zone

                            else
                                identity
                           )
                        |> Time.posixToMillis

                newSchedule =
                    Dict.insert eventMillis model.currentDescInput model.schedule
            in
            ( { model
                | schedule = newSchedule
                , currentHourInput = "00"
                , currentMinutesInput = "00"
                , currentDescInput = ""
              }
            , sendToBackend <| ScheduleChanged model.currentPoolname newSchedule model.time
            )

        DeleteEventPressed millis ->
            let
                newSchedule =
                    Dict.remove millis model.schedule
            in
            ( { model | schedule = newSchedule }
            , sendToBackend <| ScheduleChanged model.currentPoolname newSchedule model.time
            )

        PoolnameInputChanged text ->
            ( { model | currentPoolname = text }
            , Cmd.none
            )

        AddToPoolRequested ->
            ( model
            , sendToBackend <| JoinPool model.currentPoolname model.schedule model.time
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewSchedule schedule ->
            ( { model | schedule = schedule }
            , Cmd.none
            )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize Resized
        ]


colors : { foreground : Color, background : Color }
colors =
    { foreground = rgb255 241 241 230
    , background = rgb255 21 35 65
    }


view : Model -> Html FrontendMsg
view model =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Bg.color colors.background
        , Events.onMouseEnter <| MouseOver True
        , Events.onMouseLeave <| MouseOver False
        ]
    <|
        column
            -- main column
            [ width fill
            , height fill
            ]
            [ column
                -- clock and date
                (if model.scheduleShown then
                    [ padding <| round <| Rel.size model.size MoonClockPadding ]

                 else
                    [ width fill
                    , spacing (Rel.size model.size MainSpacing |> round)
                    , centerY
                    ]
                )
                [ Input.button
                    [ if model.scheduleShown then
                        paddingXY (round <| Rel.size model.size MoonClockPadding) 0

                      else
                        centerX
                    ]
                    { label =
                        el []
                            (Clock.new
                                { size =
                                    Rel.size model.size <|
                                        if model.scheduleShown then
                                            MoonClock

                                        else
                                            Clock
                                , zone = model.zone
                                , now = model.time
                                , faceColor = colors.foreground
                                , handColor = colors.background
                                }
                                |> Clock.withEvents
                                    (Dict.keys model.schedule
                                        |> List.map Time.millisToPosix
                                    )
                                |> Clock.view
                            )
                    , onPress = Just ScheduleToggled
                    }
                , Input.button [ width fill ]
                    { label = viewDate model
                    , onPress =
                        if model.scheduleShown then
                            Just ScheduleToggled

                        else
                            Just DateToggled
                    }
                ]
            , if model.scheduleShown then
                column
                    [ width fill
                    , height fill
                    , Bg.color <| rgb255 0 15 8
                    ]
                    [ column
                        -- schedule
                        [ width fill
                        , padding <| round <| Rel.size model.size SchedulePadding
                        , Font.color <| colors.foreground
                        , Font.size <| round <| Rel.size model.size ScheduleFontSize
                        , spacing <| round <| Rel.size model.size ScheduleLineSpacing
                        ]
                        ((model.schedule
                            |> Dict.toList
                            |> List.map (viewEvent model)
                         )
                            ++ [ row
                                    [ width fill
                                    , Font.color <| rgb 0 0 0
                                    , spacing <| round <| Rel.size model.size ScheduleLineSpacing
                                    ]
                                    [ Input.text
                                        [ width <| px <| round <| Rel.size model.size ScheduleFontSize * 3
                                        , focusedOnLoad
                                        , onEnter AddEventPressed
                                        ]
                                        { onChange = HourInputChanged
                                        , text = model.currentHourInput
                                        , placeholder = Nothing
                                        , label = Input.labelHidden "Hours"
                                        }
                                    , el [ Font.color colors.foreground ] <| text ":"
                                    , Input.text
                                        [ width <| px <| round <| Rel.size model.size ScheduleFontSize * 3
                                        , onEnter AddEventPressed
                                        ]
                                        { onChange = MinutesInputChanged
                                        , text = model.currentMinutesInput
                                        , placeholder = Nothing
                                        , label = Input.labelHidden "Minutes"
                                        }
                                    , Input.text
                                        [ width fill --<| px <| round <| Rel.size model.size ScheduleFontSize * 15
                                        , onEnter AddEventPressed
                                        ]
                                        { onChange = DescInputChanged
                                        , text = model.currentDescInput
                                        , placeholder = Nothing
                                        , label = Input.labelHidden "Description"
                                        }
                                    , Input.button []
                                        { onPress = Just AddEventPressed
                                        , label = el [ Font.color <| colors.foreground, paddingXY 10 0 ] <| text "+"
                                        }
                                    ]
                               ]
                        )
                    , el [ height fill ] none
                    , Input.text
                        [ width shrink
                        , onEnter AddToPoolRequested
                        ]
                        { onChange = PoolnameInputChanged
                        , text = model.currentPoolname
                        , placeholder = Nothing
                        , label = Input.labelHidden "Poolname"
                        }
                    ]

              else
                none
            ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


viewEvent : Model -> ( Int, String ) -> Element FrontendMsg
viewEvent model ( millis, description ) =
    let
        timeParts =
            Time.millisToPosix millis
                |> Time.Extra.posixToParts model.zone
    in
    row [ spacing <| round <| Rel.size model.size ScheduleLineSpacing ]
        [ el [] <|
            text <|
                (timeParts.hour |> String.fromInt)
                    ++ ":"
                    ++ (timeParts.minute |> String.fromInt |> String.padLeft 2 '0')
                    ++ "  "
                    ++ description
        , Input.button []
            { onPress = Just <| DeleteEventPressed millis
            , label = el [] <| text "x"
            }
        ]


viewDate : Model -> Element msg
viewDate model =
    let
        ( blur, color ) =
            if model.scheduleShown || model.dateHidden && model.mouseOver then
                ( Rel.size model.size DateBlur
                    |> String.fromFloat
                  -- , rgb255 212 212 202
                , rgb255 167 167 159
                )

            else
                ( "0", colors.foreground )
    in
    el
        [ transparent <| model.dateHidden && not model.mouseOver && not model.scheduleShown
        , htmlAttribute <| Html.Attributes.style "filter" <| "blur(" ++ blur ++ "px)"
        , Font.center
        , Font.color <| color
        , Font.size <| (Rel.size model.size DateFont |> round)
        , moveUp <|
            if model.scheduleShown then
                Rel.size model.size DateCloudShift

            else
                0
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
        , centerX
        ]
    <|
        text <|
            viewDateString model


viewDateString : Model -> String
viewDateString model =
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
