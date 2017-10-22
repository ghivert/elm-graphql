module GraphQl
  exposing
    ( Operation, Value, Anonymous, Named, Variables, Mutation, Query, Request, Argument
    , query, mutation, addVariables
    , object, named, field
    , withArgument, withVariables, withSelectors, withAlias
    , variable, type_, int, string, input, nestedInput
    , send
    )

{-| GraphQL queries and mutations made easy in Elm!
This package provides easier way to deal with GraphQL queries and mutations.
This package aims to partially mimic the Json Encoders and the HTTP API.
Every user of Elm should not be lost using this package.

```
import GraphQl exposing (Mutation, Query, Variables, Named, Operation)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

-- Define the request.
userRequest : Operation Query Variables
userRequest =
  GraphQl.named "MySuperQuery"
    [ GraphQl.field "user"
      |> GraphQl.withArgument "id" (GraphQl.variable "id")
      |> GraphQl.withSelectors
        [ GraphQl.field "id"
        , GraphQl.field "name"
          |> GraphQl.withSelectors
            [ GraphQl.field "first_name"
            , GraphQl.field "last_name"
            ]
          |> GraphQl.withArgument "id" (GraphQl.type_ "INT!")
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
    |> GraphQl.withVariables [ ("id", "INT!") ]

userModifying : Operation Mutation Named
userModifying =
  GraphQl.named "MySuperMutation"
    [ GraphQl.field "CreateUser"
      |> GraphQl.withArgument "user"
        (GraphQl.input
          [ ("first", GraphQl.string "John")
          , ("last", GraphQl.string "Doe")
          ]
        )
    ]

-- And Send It!
sendRequest : Int -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
sendRequest id msg decoder =
  GraphQl.query "/example_endpoint" userRequest decoder
    |> GraphQl.addVariables [ ("id", Encode.int id) ]
    |> GraphQl.send msg
```

# Value
@docs Operation
@docs Value
@docs Anonymous
@docs Named
@docs Variables
@docs Mutation
@docs Query

# Constructors
@docs object
@docs named
@docs field

# Modifiers
@docs withArgument
@docs withVariables
@docs withAlias
@docs withSelectors

# Arguments
@docs Argument
@docs int
@docs string
@docs type_
@docs variable
@docs input
@docs nestedInput

# Requests
@docs Request
@docs query
@docs mutation
@docs addVariables
@docs send

-}

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import GraphQl.Value as Value
import Helpers



{-| Requests contains the query or the mutation and the variables of each GraphQL requests.
The variables can't be used with an anonymous Request, due to the nature of GraphQL.
-}
type Request a b c
  = Request OperationType String (Operation a b) (Decoder c) (Maybe (List (String, Encode.Value)))

{-| Entry of every GraphQL query to turn them into requests, which can be launched!

    object []
      |> flip (query "https://example.com") decoder
      |> send msg
-}
query : String -> Operation Query a -> Decoder b -> Request Query a b
query endpoint query_ decoder =
  Request OperationQuery endpoint query_ decoder Nothing

{-| Entry of every GraphQL mutation to turn them into requests, which can be launched!

    object []
      |> flip (mutation "https://example.com") decoder
      |> send msg
-}
mutation : String -> Operation Mutation a -> Decoder b -> Request Mutation a b
mutation endpoint query_ decoder =
  Request OperationMutation endpoint query_ decoder Nothing

{-| Add variables to a request. Useful when defining variables in your GraphQL request.
If no variables has been defined in your operation, no variables can be added: the
compiler will reject it.

    named [ field "user" |> withArgument "id" (variable "id") ]
      |> withVariables [ ("id", "INT") ]
      |> flip (query "https://example.com") decoder
      |> addVariables [ ("id", Encode.int 12) ]
      |> send msg
-}
addVariables : List (String, Encode.Value) -> Request a Variables b -> Request a Variables  b
addVariables variables (Request type_ endpoint operation decoder _) =
  Request type_ endpoint operation decoder (Just variables)



type OperationType
  = OperationQuery
  | OperationMutation

{-| -}
type Query
  = Query

{-| -}
type Mutation
  = Mutation

{-| -}
type Anonymous
  = Anonymous

{-| -}
type Named
  = Named

{-| -}
type Variables
  = Variables

{-| Handle GraphQL values. -}
type alias Value a
  = Value.Value a

{-| Handle GraphQL operations -}
type Operation a b
  = Operation (Value.Value a)

{-| Handle arguments on GraphQL fields. -}
type Argument a
  = Argument String

{-| Generates a Value, from a list of fields.

    object
      [ field "user" ]

Turns into:

    {
      user
    }
-}
object : List (Value a) -> Operation a Anonymous
object selectors =
  selectors
    |> Value.addSelectorsIn Value.new
    |> Operation

{-| Generates a Value with a name.

    named "MySuperRequest"
      [ field "user" ]

Turns into:

    MySuperRequest {
      user
    }
-}
named : String -> List (Value a) -> Operation a Named
named id selectors =
  selectors
    |> Value.addSelectorsIn Value.new
    |> Value.setId id
    |> Operation

{-| Generates a field. -}
field : String -> Value a
field id =
  Value.new
    |> Value.setId id

{-| Adds variables to an Operation.

    "UserRequest"
      [ field "user" ]
      |> withVariables [ ("id", "id") ]

Turns into:

    query UserRequest(id: $id) {
      user
    }
-}
withVariables : List (String, String) -> Operation a Named -> Operation a Variables
withVariables values (Operation value) =
  values
    |> List.map generateVariablePair
    |> List.foldr (flip Value.addInValueVariables) value
    |> Operation

generateVariablePair : (String, String) -> (String, String)
generateVariablePair (name, content) =
  ("$" ++ name, content)

{-| Adds selectors to a Field.

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
withSelectors : List (Value a) -> Value a -> Value a
withSelectors selectors value =
  selectors |> Value.addSelectorsIn value

{-| Adds an alias to a Field.

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
withAlias : String -> Value a -> Value a
withAlias alias value =
  value |> Value.setAlias alias

{-| Adds an argument to a Field.

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
withArgument : String -> Argument a -> Value a -> Value a
withArgument name (Argument content) value =
  (name, content) |> Value.addInValueArguments value

{-| Generates an argument, to use with `withArgument`.
You don't have to handle the $ sign.

    field "user"
      |> withArgument "id" (GraphQl.variable "id")

Turns into:

    user(id: $id)
-}
variable : String -> Argument a
variable name =
  Argument ("$" ++ name)

{-| Generates an argument, to use with `withArgument`.

    field "user"
      |> withArgument "id" (GraphQl.int 12)

Turns into:

    user(id: 12)
-}
int : Int -> Argument a
int =
  Argument << toString

{-| Generates an argument, to use with `withArgument`.

    field "user"
      |> withArgument "id" (GraphQl.string "12")

Turns into:

    user(id: "12")
-}
string : String -> Argument a
string =
  Argument << Helpers.betweenQuotes

{-| Generates an argument, to use with `withArgument`.
Generate a type in GraphQL.

    field "user"
      |> withArgument "id" (GraphQl.type_ "INT")

Turns into:

    user(id: INT)
-}
type_ : String -> Argument a
type_ =
  Argument

{-| Generates an argument, to use with 'withArgument'.

    field "CreateUser"
      |> withArgument "user"
        (GraphQl.input
          [ ("first", (GraphQl.string "John"))
          , ("last",  (GraphQl.string "Doe"))
          ]
        )

Turns into:

    CreateUser(user: {first: "John", last: "Doe"})
-}
input : List (String, Argument Mutation) -> Argument Mutation
input input =
  input
    |> inputToString
    |> Argument

{-| Generates an argument, to use with 'withArgument'.

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
nestedInput : List (List (String, Argument Mutation)) -> Argument Mutation
nestedInput nestedInput =
  nestedInput
    |> List.map inputToString
    |> String.join ", "
    |> Helpers.betweenBrackets
    |> Argument

inputToString : List (String, Argument Mutation) -> String
inputToString input =
  input
    |> List.map addInputField
    |> String.join ", "
    |> Helpers.betweenBraces

addInputField : (String, Argument Mutation) -> String
addInputField ( param, Argument operation ) =
    param ++ ": " ++ operation



{-| Sends the GraphQL request! Generates a Cmd, to feed the runtime in your update. -}
send : (Result Http.Error c -> msg) -> Request a b c -> Cmd msg
send msg =
  Http.send msg << toHttpRequest

toHttpRequest : Request a b c -> Http.Request c
toHttpRequest request =
  case request of
    Request type_ endpoint operation decoder variables ->
      Http.post endpoint
        (operationToBody type_ operation variables)
        (Decode.field "data" decoder)

operationToBody : OperationType -> Operation a b -> Maybe (List (String, Encode.Value)) -> Http.Body
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

encodeOperation : OperationType -> Operation a b -> String
encodeOperation type_ (Operation value) =
  value
    |> Value.encodeValue
    |> (++) (operationToString type_)

operationToString : OperationType -> String
operationToString type_ =
  case type_ of
    OperationMutation ->
      "mutation "
    OperationQuery ->
      "query "
