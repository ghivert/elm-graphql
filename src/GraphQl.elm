module GraphQl
  exposing
    ( Field, Request, Body
    , query
    , root, named, field
    , withIntArg, withStringArg, withSelectors, withTypeArg
    , send
    )

{-| GraphQL made easy in Elm!

# Types
@docs Body
@docs Field
@docs Request

# Constructors
@docs query
@docs root
@docs named
@docs field

# Fields modifiers
@docs withIntArg
@docs withStringArg
@docs withTypeArg
@docs withSelectors

# Send Commands
@docs send
-}

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import GraphQl.Field as Field
import Helpers

{-| -}
type Request a
  = Query String Body (Decoder a)

{-| -}
type Body =
  Body
    { fields : List Field
    , variables : List (String, String)
    , name : Maybe String
    }

{-| -}
type alias Field =
  Field.Field

{-| -}
query : String -> Body -> Decoder a -> Request a
query =
  Query

{-| -}
root : List Field -> Body
root fields =
  Body
    { fields = fields
    , variables = []
    , name = Nothing
    }

{-| -}
named : String -> List Field -> Body
named name fields =
  Body
    { fields = fields
    , variables = []
    , name = Just name
    }

{-| -}
field : String -> Field
field =
  Field.new

{-| -}
withIntArg : (String, Int) -> Field -> Field
withIntArg argument field =
  argument
    |> Tuple.mapSecond toString
    |> Field.addInFieldArgs field

{-| -}
withStringArg : (String, String) -> Field -> Field
withStringArg argument field =
  argument
    |> Tuple.mapSecond Helpers.surroundQuotes
    |> Field.addInFieldArgs field

{-| -}
withTypeArg : (String, String) -> Field -> Field
withTypeArg =
  flip Field.addInFieldArgs

{-| -}
withSelectors : List Field -> Field -> Field
withSelectors =
 Field.setSelectors

{-| -}
withName : String -> Field -> Field
withName =
  Field.setName

{-| -}
send : (Result Http.Error a -> msg) -> Request a -> Cmd msg
send msg request =
  Http.send msg <|
    toPostRequest request

toPostRequest : Request a -> Http.Request a
toPostRequest request =
  case request of
    Query endpoint body decoder ->
      Http.post endpoint
        (queryBody body)
        (decodeGraphQlQueries decoder)

queryBody : Body -> Http.Body
queryBody (Body body) =
  Http.jsonBody <|
    Encode.object
      [ ( "query", encodeQuery body.name body.fields ) ]

encodeQuery : Maybe String -> List Field -> Encode.Value
encodeQuery name =
  Encode.string << encodeQueryAsString name

encodeQueryAsString : Maybe String -> List Field -> String
encodeQueryAsString name fields =
  fields
    |> List.map Field.encodeField
    |> String.join "\n"
    |> Helpers.surroundBraces
    |> (++) ("query " ++ Maybe.withDefault "" name)

decodeGraphQlQueries : Decode.Decoder a -> Decode.Decoder a
decodeGraphQlQueries =
  Decode.field "data"
