module GraphQl
  exposing
    ( Value, Request, Argument
    , query, addVariables
    , object, named, field
    , withArgument, withVariable, withSelectors, withAlias
    , variable, type_, int, string
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

# Values modifiers
@docs withIntArg
@docs withStringArg
@docs withTypeArg
@docs withVarArg
@docs withSelectors
@docs addVariables

# Send Commands
@docs send
-}

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import GraphQl.Value as Value
import Helpers



type Request a
  = Query String Value (Decoder a) (Maybe (List (String, Encode.Value)))

{-| -}
query : String -> Value -> Decoder a -> Request a
query endpoint query_ decoder =
  Query endpoint query_ decoder Nothing

{-| -}
addVariables : List (String, Encode.Value) -> Request a -> Request a
addVariables variables request =
  case request of
    Query endpoint query_ decoder _ ->
      Query endpoint query_ decoder (Just variables)



{-| -}
type Value =
  Value Value.Value

extractValue : Value -> Value.Value
extractValue (Value field) =
  field

type Argument =
  Argument String

{-| -}
object : List Value -> Value
object selectors =
  selectors
    |> List.map extractValue
    |> Value.addSelectorsIn Value.new
    |> Value

{-| -}
named : String -> List Value -> Value
named id selectors =
  field id
    |> withSelectors selectors

{-| -}
field : String -> Value
field id =
  Value.new
    |> Value.setId id
    |> Value

withVariable : String -> String -> Value -> Value
withVariable name content (Value value) =
  ("$" ++ name, content)
    |> Value.addInValueVariables value
    |> Value

{-| -}
withSelectors : List Value -> Value -> Value
withSelectors selectors (Value value) =
  selectors
    |> List.map extractValue
    |> Value.addSelectorsIn value
    |> Value

{-| -}
withAlias : String -> Value -> Value
withAlias alias (Value value) =
  value
    |> Value.setAlias alias
    |> Value

withArgument : String -> Argument -> Value -> Value
withArgument name (Argument content) (Value value) =
  (name, content)
    |> Value.addInValueArguments value
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
send : (Result Http.Error a -> msg) -> Request a -> Cmd msg
send msg =
  Http.send msg << toHttpRequest

toHttpRequest : Request a -> Http.Request a
toHttpRequest request =
  case request of
    Query endpoint query_ decoder variables ->
      Http.post endpoint
        (queryToBody query_ variables)
        (Decode.field "data" decoder)

queryToBody : Value -> Maybe (List (String, Encode.Value)) -> Http.Body
queryToBody value variables =
  Http.jsonBody <|
    Encode.object <|
      List.concat
        [ [ ("query", Encode.string <| encodeQuery value) ]
        , variables
          |> Maybe.map Encode.object
          |> Maybe.map ((,) "variables")
          |> Maybe.map List.singleton
          |> Maybe.withDefault []
        ]

encodeQuery : Value -> String
encodeQuery (Value value) =
  value
    |> Value.encodeValue
    |> (++) "query "
