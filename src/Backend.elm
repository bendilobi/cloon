module Backend exposing (..)

import Dict
import Env
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToFrontend)
import List.Extra
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
            if poolName == Env.hardcodedPoolname then
                let
                    pool =
                        case Dict.get poolName model.pools of
                            Nothing ->
                                {- Pool doesn't exist yet so we create a new one -}
                                { sessions = [ sessionId ]
                                , schedule = schedule
                                , lastChange = Time.posixToMillis time
                                }

                            Just pl ->
                                { pl
                                    | sessions =
                                        ensurePoolMembership pl.sessions sessionId
                                    , lastChange = Time.posixToMillis time
                                }
                in
                ( { model | pools = Dict.insert poolName pool model.pools }
                , sendToFrontend clientId <| NewSchedule pool.schedule
                )

            else
                --TODO: Maybe give back some error?
                ( model, Cmd.none )

        ScheduleChanged poolName schedule time ->
            case Dict.get poolName model.pools of
                Nothing ->
                    --TODO: Maybe give back some error?
                    ( model, Cmd.none )

                Just pool ->
                    let
                        -- affectedSessions =
                        --     List.Extra.remove sessionId pool.sessions
                        changedPool =
                            { sessions = ensurePoolMembership pool.sessions sessionId
                            , schedule = schedule
                            , lastChange = Time.posixToMillis time
                            }
                    in
                    ( { model | pools = Dict.insert poolName changedPool model.pools }
                      -- , affectedSessions
                    , changedPool.sessions
                        |> List.map (\sId -> sendToFrontend sId <| NewSchedule schedule)
                        |> Cmd.batch
                    )


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch [ onConnect ClientConnected ]


{-| Make sure that the list of sessions doesn't grow indefinitely by dropping old sessions
-}
ensurePoolMembership : List SessionId -> SessionId -> List SessionId
ensurePoolMembership sessions id =
    if List.member id sessions then
        sessions

    else
        id
            :: sessions
            |> List.take 20
