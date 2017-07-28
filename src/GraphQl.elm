module GraphQl
  exposing
    ( Value, Root, Field, Request, Argument
    , query, addVariables
    , object, named, field
    , withArgument, withVariable, withSelectors, withAlias
    , variable, type_, int, string
    , send
    )

{-| GraphQL queries made easy in Elm!
This package provides easier way to deal with GraphQL queries.
This package aims to partially mimic the Json Encoders and the HTTP API.
Every user of Elm should not be lost using this package.

```
import GraphQl

-- Define the request.
userRequest : GraphQl.Value
userRequest =
  GraphQl.object
    [ GraphQl.field "user"
      |> GraphQl.withArgument "id" (GraphQl.variable "id")
      |> GraphQl.withSelectors
        [ GraphQl.field "id"
        , GraphQl.field "name"
          |> GraphQl.withSelectors
            [ GraphQl.field "first_name"
            , GraphQl.field "last_name"
            ]
        ]
    , GraphQl.field "address"
      |> GraphQl.withArgument "city" (GraphQl.string "Paris")
      |> GraphQl.withArgument "id" (GraphQl.int 12)
      |> GraphQl.withArgument "type" (GraphQl.type_ "LOFT")
      |> GraphQl.withSelectors
        [ GraphQl.field "street"
        , GraphQl.field "neighborhood"
        ]
    ]
    |> GraphQl.withVariable "id" "INT!"

-- And Send It!
sendRequest : Int -> Decoder a -> Cmd msg
sendRequest id decoder =
  GraphQl.query "/example_endpoint" userRequest decoder
    |> GraphQl.addVariables [ ("id", Encode.int id) ]
    |> GraphQl.send
```

# Value
@docs Value
@docs Root
@docs Field

# Constructors
@docs object
@docs named
@docs field

# Modifiers
@docs withArgument
@docs withVariable
@docs withAlias
@docs withSelectors

# Arguments
@docs Argument
@docs int
@docs string
@docs type_
@docs variable

# Requests
@docs Request
@docs query
@docs addVariables
@docs send

-}

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import GraphQl.Value as Value
import Helpers



{-| Requests contains the query and the variables of each GraphQl requests. -}
type Request a
  = Query String (Value Root) (Decoder a) (Maybe (List (String, Encode.Value)))

{-| Entry of every GraphQL values to turn them into requests, which can be launched!

    object []
      |> flip (query "https://example.com") decoder
      |> send msg
-}
query : String -> Value Root -> Decoder a -> Request a
query endpoint query_ decoder =
  Query endpoint query_ decoder Nothing

{-| Add variables to a requests. Useful when defining variables in your GraphQL request.

    object []
      |> flip (query "https://example.com") decoder
      |> addVariables []
      |> send msg
-}
addVariables : List (String, Encode.Value) -> Request a -> Request a
addVariables variables request =
  case request of
    Query endpoint query_ decoder _ ->
      Query endpoint query_ decoder (Just variables)



{-| -}
type Root =
  Root

{-| -}
type Field =
  Field

{-| Handle GraphQL values. -}
type Value a =
  Value Value.Value

extractValue : Value a -> Value.Value
extractValue (Value value) =
  value

{-| Handle arguments on GraphQL fields. -}
type Argument =
  Argument String

{-| Generate a Value, from a list of fields.

    object
      [ field "user" ]

Turns into:

    query {
      user
    }
-}
object : List (Value Field) -> Value Root
object selectors =
  selectors
    |> List.map extractValue
    |> Value.addSelectorsIn Value.new
    |> Value

{-| Generate a Value with a name.

    named "MySuperRequest"
      [ field "user" ]

Turns into:

    query MySuperRequest {
      user
    }
-}
named : String -> List (Value Field) -> Value Root
named id selectors =
  selectors
    |> List.map extractValue
    |> Value.addSelectorsIn Value.new
    |> Value.setId id
    |> Value

{-| Generate a field. -}
field : String -> Value Field
field id =
  Value.new
    |> Value.setId id
    |> Value

{-| Add a variable to a Field.

    named "UserRequest"
      [ field "user" ]
      |> withVariable "id" "id"

Turns into:

    query UserRequest(id: $id) {
      user
    }
-}
withVariable : String -> String -> Value Root -> Value Root
withVariable name content (Value value) =
  ("$" ++ name, content)
    |> Value.addInValueVariables value
    |> Value

{-| Add selectors to a Field.

    field "user"
      |> withSelectors
        [ field "id"
        , field "first_name"
        , field "last_name"
        ]

Turns into:

    user {
      id
      first_name
      last_name
    }
-}
withSelectors : List (Value Field) -> Value Field -> Value Field
withSelectors selectors (Value value) =
  selectors
    |> List.map extractValue
    |> Value.addSelectorsIn value
    |> Value

{-| Add an alias to a Field.

    field "user"
      |> withAlias "currentUser"
      |> withSelectors
        [ field "id"
        , field "first_name"
        , field "last_name"
        ]

Turns into:

    currentUser: user {
      id
      first_name
      last_name
    }
-}
withAlias : String -> Value Field -> Value Field
withAlias alias (Value value) =
  value
    |> Value.setAlias alias
    |> Value

{-| Add an argument to a Field.

    field "user"
      |> withArgument "id" (GraphQl.string "12")
      |> withSelectors
        [ field "id"
        , field "first_name"
        , field "last_name"
        ]

Turns into:

    user(id: "12") {
      id
      first_name
      last_name
    }
-}
withArgument : String -> Argument -> Value Field -> Value Field
withArgument name (Argument content) (Value value) =
  (name, content)
    |> Value.addInValueArguments value
    |> Value

{-| Generate an argument, to use with `withArgument`.
You don't have to handle the $ sign.

    fied "user"
      |> withArgument "id" (GraphQl.variable "id")

Turns into:

    user(id: $id)
-}
variable : String -> Argument
variable name =
  Argument ("$" ++ name)

{-| Generate an argument, to use with `withArgument`.

    fied "user"
      |> withArgument "id" (GraphQl.int 12)

Turns into:

    user(id: 12)
-}
int : Int -> Argument
int =
  Argument << toString

{-| Generate an argument, to use with `withArgument`.

    fied "user"
      |> withArgument "id" (GraphQl.string "12")

Turns into:

    user(id: "12")
-}
string : String -> Argument
string =
  Argument << Helpers.betweenQuotes

{-| Generate an argument, to use with `withArgument`.
Generate a type in GraphQL.

    fied "user"
      |> withArgument "id" (GraphQl.type_ "INT")

Turns into:

    user(id: INT)
-}
type_ : String -> Argument
type_ =
  Argument



{-| Actually send the GraphQL request! Generates a Cmd, to give to Elm in your update. -}
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

queryToBody : Value a -> Maybe (List (String, Encode.Value)) -> Http.Body
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

encodeQuery : Value a -> String
encodeQuery (Value value) =
  value
    |> Value.encodeValue
    |> (++) "query "
