module GraphQl.Http exposing (Options, send)

{-| This module provides helpers to directly deal with HTTP with GraphQL.

@docs Options
@docs send
-}

import GraphQl
import Json.Decode as Decode exposing (Decoder)
import Http exposing (Error)

{-| Defines all the options for a GraphQl request. -}
type alias Options =
  { url : String
  , headers : List Http.Header
  }

{-| Send a GraphQL request directly through an HTTP post, directly decoding the
`data` field in the response.

    graphQlRequestOptions : GraphQl.Options
    graphQlRequestOptions =
      { url = "/example_endpoint"
      , headers = []
      }

    sendMyAwesomeRequest : (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
    sendMyAwesomeRequest msg decoder =
      GraphQl.query myAwesomeRequest
      |> GraphQl.Http.send graphQlRequestOptions msg decoder
-}
send : Options -> (Result Error c -> msg) -> Decoder c -> GraphQl.Request a b -> Cmd msg
send { url, headers } msg decoder body =
  { method = "POST"
  , headers = headers
  , url = url
  , body = Http.jsonBody (GraphQl.toJson body)
  , timeout = Nothing
  , tracker = Nothing
  , expect = Http.expectJson msg (Decode.field "data" decoder)
  }
  |> Http.request
