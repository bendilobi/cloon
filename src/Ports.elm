port module Ports exposing (InitData, ToElm(..), decodeMsg, toElm, toJs)

import Json.Decode
import Json.Encode


type ToElm
    = NoOp
    | UnknownMessage String
    | GotInitData InitData


type alias InitData =
    { poolName : String
    , version : String
    }


port toElm : (String -> msg) -> Sub msg


decodeMsg : String -> ToElm
decodeMsg json =
    case Json.Decode.decodeString (Json.Decode.field "tag" Json.Decode.string) json of
        Ok "NoOp" ->
            NoOp

        Ok "InitData" ->
            case Json.Decode.decodeString initDataDecoder json of
                Ok initData ->
                    GotInitData initData

                Err message ->
                    UnknownMessage (formatError "initData" message)

        Err message ->
            UnknownMessage (formatError "tag" message)

        Ok tagname ->
            UnknownMessage <| "tag is unknown: " ++ tagname


formatError : String -> Json.Decode.Error -> String
formatError field error =
    "Could not decode" ++ field ++ ": " ++ Json.Decode.errorToString error


port toJs : { tag : String, data : Json.Encode.Value } -> Cmd msg


initDataDecoder : Json.Decode.Decoder InitData
initDataDecoder =
    let
        poolNameDecoder : Json.Decode.Decoder String
        poolNameDecoder =
            Json.Decode.field "poolName" Json.Decode.string

        versionDecoder : Json.Decode.Decoder String
        versionDecoder =
            Json.Decode.field "version" Json.Decode.string
    in
    Json.Decode.field "data"
        (Json.Decode.map2 InitData
            poolNameDecoder
            versionDecoder
        )
