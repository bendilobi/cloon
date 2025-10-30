module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Components.Clock as Clock
import Dict
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (focusedOnLoad)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode
import Lamdera exposing (sendToBackend)
import Platform.Cmd as Cmd
import Ports
import Set
import SizeRelations as Rel exposing (SizeRelation(..))
import String exposing (toInt)
import String.Extra
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
      , version = "-"
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , size = 200
      , dateHidden = True
      , mouseOver = False
      , schedule = { schedule = Dict.empty, lastChanged = Time.millisToPosix 0 }
      , scheduleShown = False
      , currentHourInput = ""
      , currentMinutesInput = ""
      , currentDescInput = ""
      , currentPoolnameInput = ""
      , poolName = ""
      , poolNameShown = False
      , mouseHoveringOver = Nothing
      , eventInputFocused = False
      , eventReadyForAdding = False
      , deletedEvents = Set.empty
      , addTimeListShown = False
      , hoveringOverIncrement = Nothing
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

        GotPortMessage rawMessage ->
            case Ports.decodeMsg rawMessage of
                Ports.GotInitData data ->
                    ( { model
                        | poolName = data.poolName
                        , currentPoolnameInput = data.poolName
                        , version = data.version
                      }
                    , sendToBackend <| JoinPool Nothing data.poolName model.schedule model.time
                    )

                Ports.NoOp ->
                    ( model, Cmd.none )

                Ports.UnknownMessage message ->
                    --TODO: Log error message?
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
            if model.scheduleShown then
                --About to hide the schedule
                let
                    schedule =
                        model.schedule

                    cleanedSchedule =
                        { schedule
                            | schedule = Dict.filter (\key _ -> not (Set.member key model.deletedEvents)) schedule.schedule
                            , lastChanged = model.time
                        }
                in
                ( { model
                    | scheduleShown = not model.scheduleShown
                    , schedule = cleanedSchedule
                    , deletedEvents = Set.empty
                  }
                , if Set.isEmpty model.deletedEvents then
                    Cmd.none

                  else
                    sendToBackend <| ScheduleChanged model.poolName cleanedSchedule model.time
                )

            else
                --About to show the schedule
                let
                    schedule =
                        model.schedule

                    ( upcomingEvents, pastEvents ) =
                        schedule.schedule
                            |> Dict.partition (\millis _ -> millis >= (Time.posixToMillis model.time - (Clock.eventHotTime * 60000 |> round) // 2))

                    cleanedSchedule =
                        { schedule
                            | schedule = upcomingEvents

                            -- , lastChanged = model.time
                        }
                in
                ( { model
                    | scheduleShown = not model.scheduleShown
                    , schedule = cleanedSchedule
                    , currentHourInput = ""
                    , currentMinutesInput = ""
                    , currentDescInput = ""
                  }
                  -- , Cmd.batch
                  --     [ if Dict.size pastEvents > 0 then
                  --         sendToBackend <| ScheduleChanged model.poolName cleanedSchedule model.time
                  --       else
                  --         Cmd.none
                  --     , Browser.Dom.focus ids.hoursInput |> Task.attempt (\_ -> NoOpFrontendMsg)
                  --     ]
                , Browser.Dom.focus ids.hoursInput |> Task.attempt (\_ -> NoOpFrontendMsg)
                )

        HourInputChanged newHour ->
            let
                ready =
                    case toInt newHour of
                        Nothing ->
                            False

                        Just hours ->
                            hours > 2 || newHour == "00"
            in
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
                , eventReadyForAdding = ready
              }
            , if ready then
                Browser.Dom.focus ids.minutesInput |> Task.attempt (\_ -> NoOpFrontendMsg)

              else
                Cmd.none
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
            , if String.length newMinutes > 1 then
                Browser.Dom.focus ids.descInput |> Task.attempt (\_ -> NoOpFrontendMsg)

              else
                Cmd.none
            )

        DescInputChanged newDesc ->
            ( { model
                | currentDescInput =
                    if String.length newDesc > maxDescriptionCharacters then
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

                schedule =
                    model.schedule

                newSchedule =
                    { schedule
                        | schedule = Dict.insert eventMillis (model.currentDescInput |> String.Extra.clean) schedule.schedule
                        , lastChanged = model.time
                    }
            in
            ( { model
                | schedule = newSchedule
                , currentHourInput = ""
                , currentMinutesInput = ""
                , currentDescInput = ""
                , eventReadyForAdding = False
              }
            , Cmd.batch
                [ sendToBackend <| ScheduleChanged model.poolName newSchedule model.time
                , Browser.Dom.focus ids.hoursInput |> Task.attempt (\_ -> NoOpFrontendMsg)
                ]
            )

        DeleteEventPressed millis ->
            ( { model
                | deletedEvents =
                    if Set.member millis model.deletedEvents then
                        Set.remove millis model.deletedEvents

                    else
                        Set.insert millis model.deletedEvents
              }
            , Cmd.none
            )

        PoolnameInputChanged text ->
            ( { model | currentPoolnameInput = text }
            , Cmd.none
            )

        AddToPoolRequested ->
            let
                poolName =
                    model.currentPoolnameInput |> String.Extra.clean
            in
            ( { model
                | currentPoolnameInput = poolName
                , poolName = poolName
                , poolNameShown = False
              }
            , Cmd.batch
                [ sendToBackend <| JoinPool (Just model.poolName) poolName model.schedule model.time
                , Ports.toJs { tag = "StoreSessionPoolName", data = Json.Encode.string model.currentPoolnameInput }
                ]
            )

        PoolNameInputToggled ->
            ( { model | poolNameShown = not model.poolNameShown }
            , Browser.Dom.focus ids.poolInput |> Task.attempt (\_ -> NoOpFrontendMsg)
            )

        MouseEntered id ->
            ( { model | mouseHoveringOver = id }
            , Cmd.none
            )

        EventInputFocused focused ->
            ( { model | eventInputFocused = focused }
            , Cmd.none
            )

        ShowTimeList showIt ->
            ( { model | addTimeListShown = showIt }
            , Cmd.none
            )

        AddTimeIncrement inc ->
            let
                minute =
                    case inc of
                        Fifteen ->
                            15

                        Thirty ->
                            30

                        Sixty ->
                            60

                        Ninety ->
                            90

                timeParts =
                    Time.Extra.add Time.Extra.Minute minute model.zone model.time
                        |> Time.Extra.posixToParts model.zone
            in
            ( { model
                | currentHourInput = timeParts.hour |> String.fromInt
                , currentMinutesInput = timeParts.minute |> String.fromInt |> String.padLeft 2 '0'
                , addTimeListShown = False
                , eventReadyForAdding = True
              }
            , Browser.Dom.focus ids.descInput |> Task.attempt (\_ -> NoOpFrontendMsg)
            )

        MouseOverIncrement inc ->
            ( { model | hoveringOverIncrement = inc }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        Connected ->
            ( model
            , sendToBackend <| JoinPool Nothing model.poolName model.schedule model.time
            )

        NewSchedule schedule ->
            ( { model | schedule = schedule }
            , Cmd.none
            )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize Resized
        , Ports.toElm GotPortMessage
        ]


colors : { foreground : Color, background : Color, schedule : Color, accent : Color, disabled : Color }
colors =
    { foreground = rgb255 241 241 230
    , background = rgb255 21 35 65
    , schedule = rgb255 0 15 8
    , accent = rgb255 187 136 0
    , disabled = rgb 0.2 0.2 0.2
    }


maxDescriptionCharacters =
    40


ids =
    { hoursInput = "hours"
    , minutesInput = "minutes"
    , descInput = "desc"
    , poolInput = "pool"
    }


view : Model -> Html FrontendMsg
view model =
    let
        inputStyling =
            [ Bg.color <| rgba 0 0 0 0
            , Font.color colors.foreground
            , Border.color colors.disabled
            , Border.widthEach { bottom = round <| Rel.size model.size InputFieldUnderline, top = 0, left = 0, right = 0 }
            , focused [ Border.color colors.accent ]
            , paddingXY 0 (round <| Rel.size model.size InputFieldPaddingY)
            , htmlAttribute <| Html.Attributes.autocomplete False
            ]

        inputAttributes =
            onEnter AddEventPressed :: inputStyling

        schedule =
            model.schedule.schedule
    in
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
        , Font.family
            [ Font.typeface "Verdana"
            , Font.sansSerif
            ]
        ]
    <|
        column
            -- main column
            [ width fill
            , height fill
            ]
            [ row
                (if model.scheduleShown then
                    [ width fill
                    , padding <| round <| Rel.size model.size MoonClockPadding
                    ]

                 else
                    [ width fill
                    , centerY
                    ]
                )
                [ column
                    -- clock and date
                    (if model.scheduleShown then
                        []

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
                                        (Dict.keys schedule
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
                    row
                        [ width fill
                        , Font.size <| round <| Rel.size model.size ScheduleFontSize
                        ]
                        [ el [ width fill ] none
                        , el
                            [ onLeft <|
                                if model.poolNameShown then
                                    Input.currentPassword
                                        (inputStyling
                                            ++ [ onEnter AddToPoolRequested
                                               , htmlAttribute <| Html.Attributes.id ids.poolInput
                                               , width <| px <| round <| Rel.size model.size ScheduleFontSize * 11
                                               , centerY
                                               ]
                                        )
                                        { onChange = PoolnameInputChanged
                                        , text = model.currentPoolnameInput
                                        , placeholder = Nothing
                                        , label = Input.labelHidden "Poolname"
                                        , show = False
                                        }

                                else
                                    none
                            ]
                          <|
                            Input.button
                                [ Font.color colors.foreground
                                ]
                                { onPress = Just PoolNameInputToggled
                                , label =
                                    el [ padding (round <| Rel.size model.size ButtonPadding) ] <|
                                        (FeatherIcons.share2
                                            |> FeatherIcons.withSize (Rel.size model.size ScheduleFontSize)
                                            |> FeatherIcons.toHtml []
                                            |> html
                                        )
                                }
                        ]

                  else
                    none
                ]
            , if model.scheduleShown then
                column
                    [ width fill
                    , height fill
                    , Bg.color colors.schedule
                    ]
                    [ column
                        -- schedule
                        [ width fill
                        , height fill
                        , padding <| round <| Rel.size model.size SchedulePadding
                        , Font.color <| colors.foreground
                        , Font.size <| round <| Rel.size model.size ScheduleFontSize
                        , spacing <| round <| Rel.size model.size ScheduleLineSpacing
                        , clip
                        , scrollbarY

                        {- This fixes a problem with recent versions of Chromium/Chrome
                           (see https://github.com/mdgriffith/elm-ui/issues/367)
                        -}
                        , htmlAttribute <| Html.Attributes.style "min-height" "auto"
                        ]
                        (schedule
                            |> Dict.toList
                            |> List.map (viewEvent model)
                        )
                    , el
                        [ width fill
                        , above <|
                            let
                                focusHandlingAttrs =
                                    [ Events.onFocus <| EventInputFocused True
                                    , Events.onLoseFocus <| EventInputFocused False
                                    ]
                            in
                            row
                                {- Event entry fields -}
                                [ width fill
                                , spacing 0
                                , Bg.color <| rgba 0 0 0 1 --0.6

                                -- , htmlAttribute <| Html.Attributes.attribute "style" "backdrop-filter: blur(10px);"
                                , paddingXY (round <| Rel.size model.size SchedulePadding * 0.2)
                                    (round <| Rel.size model.size EventInputPaddingY)
                                , Font.size <| round <| Rel.size model.size ScheduleFontSize
                                ]
                                [ Input.button
                                    [ inFront <|
                                        if model.addTimeListShown then
                                            viewAddTimeList model

                                        else
                                            none
                                    , Events.onMouseEnter <| ShowTimeList True
                                    , Events.onMouseLeave <| ShowTimeList False
                                    ]
                                    { onPress =
                                        if model.addTimeListShown then
                                            Nothing

                                        else
                                            Just <| ShowTimeList True
                                    , label =
                                        el
                                            [ paddingXY (round <| Rel.size model.size ButtonPadding) 0
                                            , Font.color colors.disabled
                                            ]
                                        <|
                                            (FeatherIcons.plus
                                                |> FeatherIcons.withSize (Rel.size model.size ScheduleFontSize)
                                                |> FeatherIcons.toHtml []
                                                |> html
                                            )
                                    }
                                , Input.text
                                    (inputAttributes
                                        ++ focusHandlingAttrs
                                        ++ [ width <| px <| round <| Rel.size model.size ScheduleFontSize * 1.4
                                           , Font.alignRight
                                           , htmlAttribute <| Html.Attributes.id ids.hoursInput
                                           ]
                                    )
                                    { onChange = HourInputChanged
                                    , text = model.currentHourInput
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "Hours"
                                    }
                                , el
                                    (inputAttributes
                                        ++ [ Font.color <|
                                                if model.eventInputFocused then
                                                    colors.foreground

                                                else
                                                    colors.disabled
                                           , Border.color <| rgba 0 0 0 0
                                           ]
                                    )
                                  <|
                                    text ":"
                                , Input.text
                                    (inputAttributes
                                        ++ focusHandlingAttrs
                                        ++ [ width <| px <| round <| Rel.size model.size ScheduleFontSize * 1.4
                                           , Font.alignLeft
                                           , htmlAttribute <| Html.Attributes.id ids.minutesInput
                                           ]
                                    )
                                    { onChange = MinutesInputChanged
                                    , text = model.currentMinutesInput
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "Minutes"
                                    }
                                , el [ width <| px <| round <| Rel.size model.size ScheduleLineSpacing ] none
                                , Input.text
                                    (inputAttributes
                                        ++ focusHandlingAttrs
                                        ++ [ width fill
                                           , htmlAttribute <| Html.Attributes.id ids.descInput
                                           ]
                                    )
                                    { onChange = DescInputChanged
                                    , text = model.currentDescInput
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "Description"
                                    }
                                , Input.button
                                    [ Font.color <|
                                        if model.eventReadyForAdding then
                                            colors.foreground

                                        else
                                            colors.disabled
                                    ]
                                    { onPress =
                                        if model.eventReadyForAdding then
                                            Just AddEventPressed

                                        else
                                            Nothing
                                    , label =
                                        el [ paddingXY (round <| Rel.size model.size ButtonPadding) 0 ] <|
                                            (FeatherIcons.cornerDownLeft
                                                |> FeatherIcons.withSize (Rel.size model.size ScheduleFontSize)
                                                |> FeatherIcons.toHtml []
                                                |> html
                                            )
                                    }
                                ]
                        ]
                        none
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


viewAddTimeList : Model -> Element FrontendMsg
viewAddTimeList model =
    let
        size =
            Rel.size model.size

        options =
            [ Fifteen, Thirty, Sixty, Ninety ]

        item inc =
            Input.button
                [ Events.onMouseEnter <| MouseOverIncrement <| Just inc
                , Events.onMouseLeave <| MouseOverIncrement <| Nothing
                ]
                { onPress = Just <| AddTimeIncrement inc
                , label =
                    el
                        [ Font.size <| round <| size AddTimeListFont
                        , Font.color <|
                            case model.hoveringOverIncrement of
                                Nothing ->
                                    colors.foreground

                                Just i ->
                                    if i == inc then
                                        colors.accent

                                    else
                                        colors.foreground
                        ]
                    <|
                        text <|
                            case inc of
                                Fifteen ->
                                    "+15"

                                Thirty ->
                                    "+30"

                                Sixty ->
                                    "+60"

                                Ninety ->
                                    "+90"
                }
    in
    options
        |> List.map item
        |> column
            [ Bg.color <| rgba 0 0 0 0.6
            , htmlAttribute <| Html.Attributes.attribute "style" "backdrop-filter: blur(10px);"
            , moveUp
                ((size ScheduleFontSize * (List.length options - 1 |> toFloat))
                    + ((List.length options - 2 |> toFloat) * size AddTimeListSpacing)
                    + size AddTimeListPadding
                )
            , Border.rounded <| round <| size RoundedBorder
            , Font.color colors.foreground
            , padding <| round <| size AddTimeListPadding
            , spacing <| round <| size AddTimeListSpacing
            , centerX
            ]


viewEvent : Model -> ( Int, String ) -> Element FrontendMsg
viewEvent model ( millis, description ) =
    let
        timeParts =
            Time.millisToPosix millis
                |> Time.Extra.posixToParts model.zone

        deleteButtonLabel =
            el
                [ paddingEach { right = Rel.size model.size ScheduleFontSize |> round, left = 0, top = 0, bottom = 0 }
                , transparent <| model.mouseHoveringOver /= Just millis
                ]
            <|
                -- text "x"
                ((if Set.member millis model.deletedEvents then
                    FeatherIcons.rotateCcw

                  else
                    FeatherIcons.delete
                 )
                    |> FeatherIcons.withSize (Rel.size model.size ScheduleFontSize)
                    |> FeatherIcons.toHtml []
                    |> html
                )
    in
    row
        [ spacing <| round <| Rel.size model.size ScheduleLineSpacing
        , Events.onMouseEnter <| MouseEntered <| Just millis
        , Events.onMouseLeave <| MouseEntered Nothing
        , width fill
        ]
        [ row
            (if Set.member millis model.deletedEvents then
                [ width fill, Font.strike ]

             else
                [ width fill ]
            )
            [ el [ alignTop, Font.color colors.schedule ] <|
                if timeParts.hour < 10 then
                    text "0"

                else
                    none
            , el [ alignTop ] <|
                text <|
                    (timeParts.hour |> String.fromInt)
                        ++ ":"
                        ++ (timeParts.minute |> String.fromInt |> String.padLeft 2 '0')
                        ++ "  "
            , paragraph [ width fill, alignTop ] [ text description ]
            ]
        , if model.mouseHoveringOver == Just millis then
            Input.button []
                { onPress = Just <| DeleteEventPressed millis
                , label = deleteButtonLabel
                }

          else
            deleteButtonLabel
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
