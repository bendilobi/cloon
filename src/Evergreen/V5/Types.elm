module Evergreen.V5.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Lamdera
import Time
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , zone : Time.Zone
    , time : Time.Posix
    , size : Float
    , dateHidden : Bool
    , mouseOver : Bool
    }


type alias BackendModel =
    { message : String
    , dateHidden : Bool
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


type ToBackend
    = NoOpToBackend
    | DateHiddenChanged Bool


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | NewDateHidden Bool
