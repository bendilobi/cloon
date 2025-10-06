module Frontend exposing (..)

-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Components.Clock as Clock
import Element as Ui
import Element.Background as Bg
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Lamdera exposing (sendToBackend)
import Platform.Cmd as Cmd
import String.Format
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
      , mouseOver = False
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

        MouseOver over ->
            ( { model | mouseOver = over }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewDateHidden isHidden ->
            ( { model | dateHidden = isHidden }
            , Cmd.none
            )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize Resized
        ]


colors : { foreground : Ui.Color, background : Ui.Color }
colors =
    { foreground = Ui.rgb255 241 241 230
    , background = Ui.rgb255 21 35 65
    }


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
            , Events.onMouseEnter <| MouseOver True
            , Events.onMouseLeave <| MouseOver False
            ]
        <|
            Ui.column
                [ Ui.width Ui.fill
                , Ui.spacing (model.size * 0.08 |> round)
                , Ui.centerY
                ]
                [ Ui.el [ Ui.centerX ]
                    (Clock.new
                        { size = model.size * 0.6
                        , zone = model.zone
                        , now = model.time
                        , foreground = colors.foreground
                        , background = colors.background
                        }
                        |> Clock.view
                    )
                , let
                    blur =
                        if model.dateHidden && model.mouseOver then
                            -- "17"
                            model.size
                                * 0.03
                                |> String.fromFloat

                        else
                            "0"
                  in
                  Input.button [ Ui.width Ui.fill ]
                    { label =
                        Ui.el
                            [ Ui.transparent <| model.dateHidden && not model.mouseOver
                            , Ui.htmlAttribute <| Html.Attributes.style "filter" <| "blur(" ++ blur ++ "px)"
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
                    , onPress = Just DateToggled
                    }
                ]


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
