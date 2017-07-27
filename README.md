# Elm GraphQL

## Alternatives
If you're searching a complete solution including Decoders defined with your query, take a look at [Elm GraphQL in Elm](https://github.com/jamesmacaulay/elm-graphql), and if you're searching for converting .graphql files to Elm, take a look at [GraphQL to Elm](https://github.com/jahewson/elm-graphql)!

## Opinion
Just import GraphQL, and write queries! This package suppose your decoders are already written, and do not write decoders. It only provide a nice syntax to do GraphQL queries, and decode the `"data"` at the root of standard GraphQL for you. Just think on your schema, and don't bother with everything else. By not writing custom decoders, you can make multiple queries on the same data, with different schemas each times. They will always be converted to the same type, avoiding you to rewrote a type for each request like others can do. Moreover, it is purely written in Elm, avoiding you to think to recompile .graphql files.

## How to use?
```elm
module Types exposing (..)

import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string)


type Msg
  = GraphQl (Result Error a)


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
```
```elm
module Requests exposing (..)

import GraphQl exposing (Query, query, field, withIntArg, withSelectors)
import Json.Decode as Decode exposing (Decoder, field)
import Types exposing (User, Address, Msg)


type alias NameAndAddress =
  { user : User
  , address : Address
  }


decodeNameAndAddress =
  Decode.map2 NameAndAddress
    (field "user" decodeUser)
    (field "address" decodeAddress)


userRequest : Int -> Query
userRequest id =
  root
    [ field "user"
      |> withIntArg ( "id", id )
      |> withSelectors
        [ field "id"
        , field "name"
          |> withSelectors
            [ field "first_name"
            , field "last_name"
            ]
        ]
    , field "address"
      |> withIntArg ( "user_id", id )
      |> withSelectors
        [ field "street"
        , field "town"
        ]
    ]


baseRequest : Query -> Decoder a -> Request a
baseRequest =
  GraphQl.query "/example_endpoint"


sendRequest : Cmd Msg
sendRequest =
  GraphQl.send GraphQl <|
    baseRequest (userRequest 15) decodeNameAndAddress
```

Licenced BSD3, enjoy the work! GraphQL is amazingly awesome!
