module GraphQl
  exposing
    ( Value, Variables, Request
    , query
    , object, named, field
    , withArgument, withSelectors, withVariables, withName, withVariable
    , variable, type_, int, string
    , encodeQueryAsString
    , send
    )

{-| GraphQL made easy in Elm!

# Types
@docs Body
@docs Value

# Constructors
@docs query
@docs root
@docs named
@docs field

# Fields modifiers
@docs withIntArg
@docs withStringArg
@docs withTypeArg
@docs withVarArg
@docs withSelectors
@docs withVariables

# Send Commands
@docs send
-}

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import GraphQl.Field as Field
import Helpers

{-
  ████████ ██    ██ ██████  ███████ ███████
     ██     ██  ██  ██   ██ ██      ██
     ██      ████   ██████  █████   ███████
     ██       ██    ██      ██           ██
     ██       ██    ██      ███████ ███████
-}


type Variables = Variables
type Argument  = Argument String

type Request a
  = Query String Value (Decoder a) (Maybe Variables)

{-| -}
query : String -> Value -> Decoder a -> Request a
query endpoint body decoder =
  Query endpoint body decoder Nothing

{-| -}
object : List Value -> Value
object fields =
  fields
    |> List.map extractField
    |> Field.addSelectorsIn Field.new
    |> Value

{-| -}
named : String -> List Value -> Value
named id fields =
  let (Value new) = object fields in
    new
      |> Field.setId id
      |> Value

{-| -}
withVariables : List (String, String) -> Request a -> Request a
withVariables variables request =
  case request of
    Query endpoint body decoder _ ->
      Query endpoint body decoder (Just Variables)

{-| -}
type Value =
  Value Field.Field

extractField : Value -> Field.Field
extractField (Value field) =
  field


{-
  ███████ ██ ███████ ██      ██████
  ██      ██ ██      ██      ██   ██
  █████   ██ █████   ██      ██   ██
  ██      ██ ██      ██      ██   ██
  ██      ██ ███████ ███████ ██████
-}


{-| -}
field : String -> Value
field id =
  Field.new
    |> Field.setId id
    |> Value

withArgument : String -> Argument -> Value -> Value
withArgument name (Argument value) (Value field) =
  (name, value)
    |> Field.addInFieldArgs field
    |> Value

withVariable : String -> String -> Value -> Value
withVariable name value (Value field) =
  ("$" ++ name, value)
    |> Field.addInFieldVars field
    |> Value

variable : String -> Argument
variable name =
  Argument ("$" ++ name)

int : Int -> Argument
int =
  Argument << toString

string : String -> Argument
string =
  Argument << Helpers.surroundQuotes

type_ : String -> Argument
type_ =
  Argument

{-| -}
withSelectors : List Value -> Value -> Value
withSelectors fields (Value field) =
  fields
    |> List.map extractField
    |> Field.addSelectorsIn field
    |> Value

{-| -}
withName : String -> Value -> Value
withName name (Value field) =
  field
    |> Field.setName name
    |> Value


{-
  ███████ ███████ ███    ██ ██████
  ██      ██      ████   ██ ██   ██
  ███████ █████   ██ ██  ██ ██   ██
       ██ ██      ██  ██ ██ ██   ██
  ███████ ███████ ██   ████ ██████
-}


{-| -}
send : (Result Http.Error a -> msg) -> Request a -> Cmd msg
send msg =
  Http.send msg << toPostRequest

toPostRequest : Request a -> Http.Request a
toPostRequest request =
  case request of
    Query endpoint body decoder variables ->
      Http.post endpoint
        (queryBody body variables)
        (decodeGraphQlQueries decoder)

queryBody : Value -> Maybe Variables -> Http.Body
queryBody body variables =
  Http.jsonBody <|
    Encode.object
      [ ("query", encodeQuery body)
      -- , ("variables", encodeVariables body.variables)
      ]

encodeQuery : Value -> Encode.Value
encodeQuery =
  Encode.string << encodeQueryAsString

encodeQueryAsString : Value -> String
encodeQueryAsString (Value fields) =
  fields
    |> Field.encodeField
    |> (++) ("query " )

-- encodeVariables : List (String, String) -> Encode.Value
-- encodeVariables variables =
--   Encode.object (List.map encodeVariable)

decodeGraphQlQueries : Decode.Decoder a -> Decode.Decoder a
decodeGraphQlQueries =
  Decode.field "data"
