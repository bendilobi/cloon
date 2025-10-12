module Evergreen.V7.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type alias Schedule =
    Dict.Dict Int String


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , zone : Time.Zone
    , time : Time.Posix
    , size : Float
    , dateHidden : Bool
    , mouseOver : Bool
    , schedule : Schedule
    , scheduleShown : Bool
    , currentHourInput : String
    , currentMinutesInput : String
    , currentDescInput : String
    , currentPoolname : String
    }


type alias Pool =
    { sessions : List Lamdera.SessionId
    , schedule : Schedule
    , lastChange : Int
    }


type alias BackendModel =
    { pools : Dict.Dict String Pool
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ViewportReceived Browser.Dom.Viewport
    | Resized Int Int
    | DateToggled
    | MouseOver Bool
    | ScheduleToggled
    | HourInputChanged String
    | MinutesInputChanged String
    | DescInputChanged String
    | AddEventPressed
    | DeleteEventPressed Int
    | PoolnameInputChanged String
    | AddToPoolRequested


type ToBackend
    = NoOpToBackend
    | JoinPool String Schedule Time.Posix
    | ScheduleChanged String Schedule Time.Posix


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | NewSchedule Schedule
