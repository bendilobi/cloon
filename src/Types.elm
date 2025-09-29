module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , zone : Time.Zone
    , time : Time.Posix
    , size : Float
    , dateHidden : Bool
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ViewportReceived Browser.Dom.Viewport
    | Resized Int Int
    | DateToggled


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
