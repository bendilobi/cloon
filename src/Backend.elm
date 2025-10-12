module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToFrontend)
import Time
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
    ( { pools = Dict.empty
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
            , Cmd.none
              --TODO: Client mitteilen, dass verbunden
              -- , Cmd.batch
              --     [ sendToFrontend clientId <| NewDateHidden model.dateHidden
              --     ]
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        JoinPool poolName schedule time ->
            let
                pool =
                    case Dict.get poolName model.pools of
                        Nothing ->
                            { sessions = [ sessionId ]
                            , schedule = schedule
                            , lastChange = Time.posixToMillis time
                            }

                        Just pl ->
                            { pl
                                | sessions =
                                    if List.member sessionId pl.sessions then
                                        pl.sessions

                                    else
                                        sessionId :: pl.sessions
                                , lastChange = Time.posixToMillis time
                            }
            in
            ( { model | pools = Dict.insert poolName pool model.pools }
            , sendToFrontend clientId <| NewSchedule pool.schedule
            )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch [ onConnect ClientConnected ]
