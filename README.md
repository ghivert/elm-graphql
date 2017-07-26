# Elm GraphQL

Elm GraphQL queries made easy in Elm.

Just import GraphQL, and write queries!

```elm
import GraphQl exposing (withIntArg, withSelectors)
import Http exposing (Error)
import Json.Decode exposing (Decoder)

type Msg
  = GraphQl (Result Error a)

userRequest : Int -> Query
userRequest id =
  GraphQl.query
    [ GraphQl.field "user"
      |> withIntArg ( "id", id )
      |> withSelectors
        [ GraphQl.field "id"
        , GraphQl.field "name"
          |> withSelectors
            [ GraphQl.field "first_name"
            , GraphQl.field "last_name"
            ]
        ]
    , GraphQl.field "address"
      |> withIntArg ( "user_id", id )
      |> withSelectors
        [ GraphQl.field "street"
        , GraphQl.field "town"
        ]
    ]

sendRequest : Decoder a -> Cmd Msg
sendRequest decoder =
  GraphQl.send GraphQl "/example_endpoint" (userRequest 15) decoder

```
