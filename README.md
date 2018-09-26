# Elm GraphQL

## Opinion
Just import GraphQL, and write queries and mutations! This package suppose your decoders are already written and do not write decoders, or that you want to keep control on the data received by GraphQL response. It only provides a nice syntax to do GraphQL queries and mutations, and decode the `"data"` at the root of standard GraphQL response for you as long as you use the HTTP wrapper. Just think on your schema, and don't bother with everything else. By not writing custom decoders, you can make multiple queries on the same data, with different schemas each times. They will always be converted to the same type, avoiding you to rewrote a type for each request like others can do. Moreover, it is purely written in Elm, avoiding you to think to recompile .graphql files.  
[This post of Evan is typically explaining why you should write decoders by hand.](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6#gistcomment-2606737) This package tries to be the more agnostic possible: it's only a way to write a query in GraphQL, with a nice DSL. The idea is to allow everyone to use it with GraphQL and with whatever tool they need. If you think something specific should be added, it should remains in a `GraphQl.SpecificModule` file, like `GraphQl.Http`. Following the post of Evan, the idea in long term is to be able to generate and decode things on whatever format you want (like in [elm/bytes](https://github.com/elm/bytes) for example), and continuing to use GraphQL.

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

sendRequest : Int -> Cmd Msg
sendRequest id =
  GraphQl.query userRequest
    |> GraphQl.addVariables [ ("id", Encode.int id) ]
    |> GraphQl.send "https://example.com" GraphQlMsg decodeNameAndAddress
```

## Want to contribute?
If you think this package is good, or if you want to bugfix something, or just globally improve the package, feel free to contribute. Make a PR, open an issue, and I will gladly work on it to make it part of `elm-graphql`!

## Alternatives
If you're searching a complete solution including Decoders implicitly defined with your query, take a look at [Elm GraphQL in Elm](https://github.com/jamesmacaulay/elm-graphql), and if you're searching for converting .graphql files to Elm, take a look at [GraphQL to Elm](https://github.com/jahewson/elm-graphql)!
