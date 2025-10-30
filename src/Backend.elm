module Backend exposing (..)

import Dict
import Env
import Lamdera exposing (ClientId, SessionId, onConnect, sendToFrontend)
import Platform.Cmd as Cmd
import Time
import Time.Extra
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
            , sendToFrontend clientId <| Connected
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        JoinPool previousPoolName poolName schedule time ->
            if poolName == Env.hardcodedPoolname then
                let
                    pool =
                        case Dict.get poolName model.pools of
                            Nothing ->
                                {- Pool doesn't exist yet so we create a new one -}
                                { sessions = [ sessionId ]
                                , schedule = schedule

                                -- , lastChange = Time.posixToMillis time
                                }

                            Just pl ->
                                { pl
                                    | sessions =
                                        ensurePoolMembership pl.sessions sessionId
                                    , schedule =
                                        if
                                            (Time.Extra.compare pl.schedule.lastChanged schedule.lastChanged == GT)
                                                || Dict.isEmpty schedule.schedule
                                        then
                                            pl.schedule

                                        else
                                            schedule

                                    -- , lastChange = Time.posixToMillis time
                                }
                in
                ( { model | pools = Dict.insert poolName pool model.pools }
                  --TODO: Only broadcast if there was a change
                , pool.sessions
                    |> List.map (\sId -> sendToFrontend sId <| NewSchedule pool.schedule)
                    |> Cmd.batch
                )

            else if previousPoolName == Just Env.hardcodedPoolname then
                let
                    prevPN =
                        Env.hardcodedPoolname

                    prevPool =
                        Dict.get prevPN model.pools
                            |> Maybe.andThen
                                (\prevP -> Just { prevP | sessions = List.filter (\s -> s /= sessionId) prevP.sessions })
                in
                case prevPool of
                    Nothing ->
                        ( model, Cmd.none )

                    Just prevP ->
                        ( { model | pools = Dict.insert prevPN prevP model.pools }
                        , Cmd.none
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
                        changedPool =
                            { sessions = ensurePoolMembership pool.sessions sessionId
                            , schedule = schedule

                            -- , lastChange = Time.posixToMillis time
                            }
                    in
                    ( { model | pools = Dict.insert poolName changedPool model.pools }
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
