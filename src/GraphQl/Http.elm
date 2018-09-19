module GraphQl.Http exposing (..)

import GraphQl
import Json.Decode exposing (Decoder)
import Http exposing (Error)

send : (Result Error c -> msg) -> String -> GraphQl.Request a b -> Decoder c -> Cmd msg
send msg endpoint body decoder =
   decoder
    |> Http.post endpoint (Http.jsonBody (GraphQl.toJson body))
    |> Http.send msg
