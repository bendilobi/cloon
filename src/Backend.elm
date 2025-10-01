module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToFrontend)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , dateHidden = True
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientConnected sessionId clientId ->
            ( model
            , Cmd.batch
                [ sendToFrontend clientId <| NewDateHidden model.dateHidden
                ]
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        DateHiddenChanged isHidden ->
            ( { model | dateHidden = isHidden }
            , broadcast <| NewDateHidden isHidden
            )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch [ Lamdera.onConnect ClientConnected ]
