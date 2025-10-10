module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import SizeRelations
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , zone : Time.Zone
    , time : Time.Posix
    , relSize : SizeRelations.SizeRelation -> Float
    , dateHidden : Bool
    , mouseOver : Bool
    }


type alias BackendModel =
    { dateHidden : Bool
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
    | MouseOver Bool


type ToBackend
    = NoOpToBackend
    | DateHiddenChanged Bool


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewDateHidden Bool
