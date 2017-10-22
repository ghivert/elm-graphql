# Elm GraphQL

## Alternatives
If you're searching a complete solution including Decoders defined with your query, take a look at [Elm GraphQL in Elm](https://github.com/jamesmacaulay/elm-graphql), and if you're searching for converting .graphql files to Elm, take a look at [GraphQL to Elm](https://github.com/jahewson/elm-graphql)!

## Opinion
Just import GraphQL, and write queries and mutations! This package suppose your decoders are already written, and do not write decoders. It only provide a nice syntax to do GraphQL queries and mutations, and decode the `"data"` at the root of standard GraphQL response for you. Just think on your schema, and don't bother with everything else. By not writing custom decoders, you can make multiple queries on the same data, with different schemas each times. They will always be converted to the same type, avoiding you to rewrote a type for each request like others can do. Moreover, it is purely written in Elm, avoiding you to think to recompile .graphql files.

This package try to be similar to Json.Encode and Http. This allow to write things more easily if you're often involved with Elm!

## How to use?
Basically, creates an object with `object`, add some fields with a list of `field`, and you're done! You can add some arguments, selectors or alias to the fields, by using the corresponding functions. The type system is here to protect you from doing anything crazy, so relax and enjoy GraphQL!

Otherwise, a (huge) example:

`Main.elm`
```elm
module Main exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string)
import Http exposing (Error)
import GraphQl exposing (Operation, Variables, Query, Named)

type Msg
  = GraphQlMsg (Result Error NameAndAddress)

type alias User =
  { id : Maybe Int
  , name : Maybe Name
  }

type alias Name =
  { firstName : Maybe String
  , lastName : Maybe String
  }

type alias Address =
  { street : Maybe String
  , town : Maybe String
  }

type alias NameAndAddress =
  { user : User
  , address : Address
  }

decodeName : Decoder Name
decodeName =
  Decode.map2 Name
    (maybe (field "first_name" string))
    (maybe (field "last_name" string))

decodeUser : Decoder User
decodeUser =
  Decode.map2 User
    (maybe (field "id" int))
    (maybe (field "name" decodeName))

decodeAddress : Decoder Address
decodeAddress =
  Decode.map2 Address
    (maybe (field "street" string))
    (maybe (field "town" string))

decodeNameAndAddress : Decoder NameAndAddress
decodeNameAndAddress =
  Decode.map2 NameAndAddress
    (field "user" decodeUser)
    (field "address" decodeAddress)


userRequest : Operation Query Variables
userRequest =
  GraphQl.named "MySuperQuery"
    [ GraphQl.field "user"
      |> GraphQl.withArgument "id" (GraphQl.variable "id")
      |> GraphQl.withAlias "current_user"
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
        , GraphQl.field "town"
        ]
    ]
    |> GraphQl.withVariables [ ("id", "123") ]

baseRequest :
  Operation Query Variables
  -> Decoder NameAndAddress
  -> GraphQl.Request Query Variables NameAndAddress
baseRequest =
  GraphQl.query "/example_endpoint"

sendRequest : Int -> Cmd Msg
sendRequest id =
  baseRequest userRequest decodeNameAndAddress
    |> GraphQl.addVariables [ ("id", Encode.int id) ]
    |> GraphQl.send GraphQlMsg
```

Licenced BSD3, enjoy the work! GraphQL is amazingly awesome!
