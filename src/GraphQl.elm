module GraphQl
  exposing
    ( Value, Anonymous, Field, Request, Argument
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
userRequest : GraphQl.Value Root
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
sendRequest : Int -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
sendRequest id msg decoder =
  GraphQl.query "/example_endpoint" userRequest decoder
    |> GraphQl.addVariables [ ("id", Encode.int id) ]
    |> GraphQl.send msg
```

# Value
@docs Value
@docs Anonymous
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
type Request a b
  = Request OperationType String (Operation a) (Decoder b) (Maybe (List (String, Encode.Value)))

{-| Entry of every GraphQL values to turn them into requests, which can be launched!

    object []
      |> flip (query "https://example.com") decoder
      |> send msg
-}
query : String -> Operation a -> Decoder b -> Request a b
query endpoint query_ decoder =
  Request Query endpoint query_ decoder Nothing

mutation : String -> Operation a -> Decoder b -> Request a b
mutation endpoint query_ decoder =
  Request Mutation endpoint query_ decoder Nothing

{-| Add variables to a requests. Useful when defining variables in your GraphQL request.

    object []
      |> flip (query "https://example.com") decoder
      |> addVariables []
      |> send msg
-}
addVariables : List (String, Encode.Value) -> Request Variables a -> Request Variables a
addVariables variables (Request type_ endpoint operation decoder _) =
  Request type_ endpoint operation decoder (Just variables)



type OperationType
  = Query
  | Mutation

{-| -}
type Anonymous
  = Anonymous

type Named
  = Named

type Variables
  = Variables
  
{-| -}
type Field
  = Field

{-| Handle GraphQL values. -}
type alias Value
  = Value.Value

type Operation a
  = Operation Value.Value

{-| Handle arguments on GraphQL fields. -}
type Argument
  = Argument String

{-| Generate a Value, from a list of fields.

    object
      [ field "user" ]

Turns into:

    query {
      user
    }
-}
object : List Value -> Operation Anonymous
object selectors =
  selectors
    |> Value.addSelectorsIn Value.new
    |> Operation

{-| Generate a Value with a name.

    named "MySuperRequest"
      [ field "user" ]

Turns into:

    query MySuperRequest {
      user
    }
-}
named : String -> List Value -> Operation Named
named id selectors =
  selectors
    |> Value.addSelectorsIn Value.new
    |> Value.setId id
    |> Operation

{-| Generate a field. -}
field : String -> Value
field id =
  Value.new
    |> Value.setId id

{-| Add a variable to a Field.

    named "UserRequest"
      [ field "user" ]
      |> withVariable "id" "id"

Turns into:

    query UserRequest(id: $id) {
      user
    }
-}
withVariable : String -> String -> Operation Named -> Operation Variables
withVariable name content (Operation value) =
  ("$" ++ name, content)
    |> Value.addInValueVariables value
    |> Operation

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
withSelectors : List Value -> Value -> Value
withSelectors selectors value =
  selectors |> Value.addSelectorsIn value

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
withAlias : String -> Value -> Value
withAlias alias value =
  value |> Value.setAlias alias

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
withArgument : String -> Argument -> Value -> Value
withArgument name (Argument content) value =
  (name, content) |> Value.addInValueArguments value

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

{-| Generate an argument, to use with 'withArgument'.

    field "CreateUser"
      |> withArgument "user"
        (GraphQl.input
          [ ("first", (GraphQl.string "John"))
          , ("last", (GraphQl.string "Doe"))
          ]
        )

Turns into:

    CreateUser(user: {first: "John", last: "Doe"})
-}
input : List (String, Argument) -> Argument
input input =
  input
    |> inputToString
    |> Argument

{-| Generate an argument, to use with 'withArgument'.

    field "CreateUser"
      |> withArgument "users"
        (GraphQl.nestedInput
          [ [ ("first", (GraphQl.string "John"))
            , ("last", (GraphQl.string "Doe"))
            ]
          , [ ("first", (GraphQl.string "Jane"))
            , ("last", (GraphQl.string "Smith"))
            ]
          ]
        )

Turns into:

    CreateUsers(users: [
      {first: "John", last: "Doe"},
      {first: "Jane", last: "Smith"}
    ])
-}
nestedInput : List (List (String, Argument)) -> Argument
nestedInput nestedInput =
  nestedInput
    |> List.map inputToString
    |> String.join ", "
    |> Helpers.betweenBrackets
    |> Argument

inputToString : List (String, Argument) -> String
inputToString input =
  input
    |> List.map addInputField
    |> String.join ", "
    |> Helpers.betweenBraces

addInputField : (String, Argument) -> String
addInputField ( param, Argument operation ) =
    param ++ ": " ++ operation


{-| Actually send the GraphQL request! Generates a Cmd, to give to Elm in your update. -}
send : (Result Http.Error a -> msg) -> Request b a -> Cmd msg
send msg =
  Http.send msg << toHttpRequest

toHttpRequest : Request b a -> Http.Request a
toHttpRequest request =
  case request of
    Request type_ endpoint operation decoder variables ->
      Http.post endpoint
        (operationToBody type_ operation variables)
        (Decode.field "data" decoder)

operationToBody : OperationType -> Operation a -> Maybe (List (String, Encode.Value)) -> Http.Body
operationToBody type_ value variables =
  Http.jsonBody
    <| Encode.object
    <| List.concat
      [ [ ("query", Encode.string <| encodeOperation type_ value) ]
      , variables
        |> Maybe.map Encode.object
        |> Maybe.map ((,) "variables")
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
      ]

encodeOperation : OperationType -> Operation a -> String
encodeOperation type_ (Operation value) =
  value
    |> Value.encodeValue
    |> (++) (operationToString type_)

operationToString : OperationType -> String
operationToString type_ =
  case type_ of
    Mutation ->
      "mutation "
    Query ->
      "query "
