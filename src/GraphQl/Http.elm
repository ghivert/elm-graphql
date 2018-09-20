module GraphQl.Http exposing (send)

{-| This module provides helpers to directly deal with HTTP with GraphQL.

@docs send
-}

import GraphQl
import Json.Decode as Decode exposing (Decoder)
import Http exposing (Error)

{-| Send a GraphQL request directly through an HTTP post, directly decoding the
`data` field in the response.

    sendMyAwesomeRequest : (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
    sendMyAwesomeRequest msg decoder =
      GraphQl.query myAwesomeRequest
        |> GraphQl.Http.send "/example_endpoint" msg decoder
-}
send : String -> (Result Error c -> msg) -> Decoder c -> GraphQl.Request a b -> Cmd msg
send endpoint msg decoder body =
   (Decode.field "data" decoder)
    |> Http.post endpoint (Http.jsonBody (GraphQl.toJson body))
    |> Http.send msg
