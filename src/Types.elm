module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type alias Schedule =
    Dict Int String


type alias FrontendModel =
    { key : Key
    , version : String
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
    , currentPoolnameInput : String
    , poolName : String
    , poolNameShown : Bool
    , mouseHoveringOver : Maybe Int
    , eventInputFocused : Bool
    , eventReadyForAdding : Bool
    }


type alias Pool =
    { sessions : List SessionId
    , schedule : Schedule
    , lastChange : Int
    }


type alias BackendModel =
    { pools : Dict String Pool
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GotPortMessage String
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
    | PoolNameInputToggled
    | MouseEntered (Maybe Int)
    | EventInputFocused Bool


type ToBackend
    = NoOpToBackend
    | JoinPool (Maybe String) String Schedule Time.Posix
    | ScheduleChanged String Schedule Time.Posix


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewSchedule Schedule
