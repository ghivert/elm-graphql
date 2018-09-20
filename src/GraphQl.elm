module GraphQl
  exposing
    ( Operation, Field, Anonymous, Named, Variables, Mutation, Query, Request, Argument
    , query, mutation, addVariables
    , object, named, field
    , withArgument, withVariables, withSelectors, withAlias
    , variable, type_, int, float, bool, string, input, nestedInput
    , toJson
    )

{-| GraphQL queries and mutations made easy in Elm!
This package provides an easier way to deal with GraphQL queries and mutations.
This package is agnostic about how you send your requests through the wire. It
could packages the request inside an HTTP post for you with `GraphQl.Http`, but
also allows you to extract the JSON and send it through whatever way you want,
WebSocket for example.

```
import GraphQl exposing (Mutation, Query, Variables, Named, Operation)
import GraphQl.Http
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

-- And Send It through HTTP!
sendRequest : Int -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
sendRequest id msg decoder =
  GraphQl.query userRequest
    |> GraphQl.addVariables [ ("id", Encode.int id) ]
    |> GraphQl.Http.send "/example_endpoint" msg decoder
```

# Field
@docs Operation
@docs Field
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
@docs bool
@docs float
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
@docs toJson
-}

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import GraphQl.Field as Field
import Helpers

{-| Requests contains the query or the mutation and the variables of each GraphQL requests.
The variables can't be used with an anonymous Request, due to the nature of GraphQL.
-}
type Request a b
  = Request OperationType (Operation a b) (Maybe (List (String, Encode.Value)))

{-| Entry of every GraphQL query to turn them into requests, which can be launched!

    object []
      |> query
      |> Http.send "https://example.com" msg decoder
-}
query : Operation Query a -> Request Query a
query query_ =
  Request OperationQuery query_ Nothing

{-| Entry of every GraphQL mutation to turn them into requests, which can be launched!

    object []
      |> mutation
      |> Http.send "https://example.com" msg decoder
-}
mutation : Operation Mutation a -> Request Mutation a
mutation query_ =
  Request OperationMutation query_ Nothing

{-| Add variables to a request. Useful when defining variables in your GraphQL request.
If no variables has been defined in your operation, no variables can be added: the
compiler will reject it.

    named [ field "user" |> withArgument "id" (variable "id") ]
      |> withVariables [ ("id", "INT") ]
      |> query
      |> addVariables [ ("id", Encode.int 12) ]
      |> toJson
-}
addVariables : List (String, Encode.Value) -> Request a b -> Request a b
addVariables variables (Request requestType operation _) =
  Request requestType operation (Just variables)

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
type alias Field a
  = Field.Field a

{-| Handle GraphQL operations -}
type Operation a b
  = Operation (Field.Field a)

{-| Handle arguments on GraphQL fields. -}
type Argument
  = Argument String

{-| Generates a Field, from a list of fields.

    object
      [ field "user" ]

Turns into:

    {
      user
    }
-}
object : List (Field a) -> Operation a Anonymous
object =
  Field.addSelectorsIn Field.new >> Operation

{-| Generates a Field with a name.

    named "MySuperRequest"
      [ field "user" ]

Turns into:

    MySuperRequest {
      user
    }
-}
named : String -> List (Field a) -> Operation a Named
named id =
  Field.addSelectorsIn Field.new
    >> Field.setId id
    >> Operation

{-| Generates a field. -}
field : String -> Field a
field id =
  Field.setId id Field.new

{-| Adds variables to an Operation.

    "UserRequest" [ field "user" ]
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
    |> List.foldr Field.addInFieldVariables value
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
withSelectors : List (Field a) -> Field a -> Field a
withSelectors selectors value =
  Field.addSelectorsIn value selectors

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
withAlias : String -> Field a -> Field a
withAlias alias_ value =
  Field.setAlias alias_ value

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
withArgument : String -> Argument -> Field a -> Field a
withArgument name (Argument content) value =
  Field.addInFieldArguments (name, content) value

{-| Generates an argument, to use with `withArgument`.
You don't have to handle the $ sign.

    field "user"
      |> withArgument "id" (GraphQl.variable "id")

Turns into:

    user(id: $id)
-}
variable : String -> Argument
variable name =
  Argument ("$" ++ name)

{-| Generates an argument, to use with `withArgument`.

    field "user"
      |> withArgument "id" (GraphQl.int 12)

Turns into:

    user(id: 12)
-}
int : Int -> Argument
int =
   String.fromInt >> Argument

{-| Generates an argument, to use with `withArgument`.

    field "user"
      |> withArgument "score" (GraphQl.float 12.1)

Turns into:

    user(id: 12)
-}
float : Float -> Argument
float =
  String.fromFloat >> Argument

{-| Generates an argument, to use with `withArgument`.

    field "user"
      |> withArgument "admin" (GraphQl.bool False)

Turns into:

    user(id: false)
-}
bool : Bool -> Argument
bool value =
  Argument <|
    case value of
      True ->
        "true"
      False ->
        "false"

{-| Generates an argument, to use with `withArgument`.

    field "user"
      |> withArgument "id" (GraphQl.string "12")

Turns into:

    user(id: "12")
-}
string : String -> Argument
string =
  Helpers.betweenQuotes >> Argument

{-| Generates an argument, to use with `withArgument`.
Generate a type in GraphQL.

    field "user"
      |> withArgument "id" (GraphQl.type_ "INT")

Turns into:

    user(id: INT)
-}
type_ : String -> Argument
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
input : List (String, Argument) -> Argument
input =
  argsToString >> Argument

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
nestedInput : List (List (String, Argument)) -> Argument
nestedInput =
  List.map argsToString
    >> String.join ", "
    >> Helpers.betweenBrackets
    >> Argument

argsToString : List (String, Argument) -> String
argsToString =
  List.map addArgField
    >> String.join ", "
    >> Helpers.betweenBraces

addArgField : (String, Argument) -> String
addArgField (param, Argument operation) =
    param ++ ": " ++ operation

{-| Extract the JSON part of a `Request` to use it into your own requests.

    sendUserRequest : Cmd msg
    sendUserRequest =
      myAwesomeRequest
        |> toJson
        |> Json.Encode.encode 0
        |> WebSocket.send
-}
toJson : Request a b -> Encode.Value
toJson (Request requestType operation variables) =
  operationToJson requestType operation variables

operationToJson : OperationType -> Operation a b -> Maybe (List (String, Encode.Value)) -> Encode.Value
operationToJson requestType value variables =
  Encode.object
    <| List.concat
      [ [ ("query", Encode.string <| encodeOperation requestType value) ]
      , variables
        |> Maybe.map Encode.object
        |> Maybe.map (Tuple.pair "variables")
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
      ]

encodeOperation : OperationType -> Operation a b -> String
encodeOperation requestType (Operation value) =
  value
    |> Field.encodeField
    |> (++) (operationToString requestType)

operationToString : OperationType -> String
operationToString requestType =
  case requestType of
    OperationMutation ->
      "mutation "
    OperationQuery ->
      "query "
