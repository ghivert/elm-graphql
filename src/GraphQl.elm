module GraphQl
    exposing
        ( Query
        , Field
        , Request
        , query
        , field
        , withIntArg
        , withStringArg
        , withSelectors
        , send
        )

import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


type Query
    = Query (List Field)


type Field
    = Field
        { id : String
        , arguments : List ( String, String )
        , selectors : List Field
        }


type Request a
    = Request
        { endpoint : String
        , query : Query
        , decoder : Decoder a
        }


request : String -> Query -> Decoder a -> Request a
request endpoint query decoder =
    Request
        { endpoint = endpoint
        , query = query
        , decoder = decoder
        }


query : List Field -> Query
query =
    Query


field : String -> Field
field id =
    Field
        { id = id
        , arguments = []
        , selectors = []
        }


withIntArg : ( String, Int ) -> Field -> Field
withIntArg arg field =
    arg
        |> Tuple.mapSecond toString
        |> addInFieldArgs field


withStringArg : ( String, String ) -> Field -> Field
withStringArg arg field =
    arg
        |> Tuple.mapSecond surroundQuotes
        |> addInFieldArgs field


withSelectors : List Field -> Field -> Field
withSelectors selectors field =
    setSelectors selectors field


send : (Result Http.Error a -> msg) -> Request a -> Cmd msg
send msg (Request request) =
    Http.send msg <|
        Http.post request.endpoint
            (body request.query)
            (decodeGraphQlQueries request.decoder)


setSelectors : List Field -> Field -> Field
setSelectors selectors (Field field) =
    Field { field | selectors = selectors }


setArguments : List ( String, String ) -> Field -> Field
setArguments arguments (Field field) =
    Field { field | arguments = arguments }


addInFieldArgs : Field -> ( String, String ) -> Field
addInFieldArgs (Field field) arg =
    setArguments (arg :: field.arguments) (Field field)


surround : String -> String -> String
surround char string =
    char ++ string ++ char


surroundQuotes : String -> String
surroundQuotes =
    surround "\""


surroundBraces : String -> String
surroundBraces string =
    "{" ++ string ++ "}"


surroundBrackets : String -> String
surroundBrackets string =
    "[" ++ string ++ "]"


surroundParen : String -> String
surroundParen string =
    "(" ++ string ++ ")"


body : Query -> Http.Body
body query =
    Http.jsonBody <|
        Encode.object
            [ ( "query", encodeQuery query ) ]


encodeQuery : Query -> Encode.Value
encodeQuery =
    Encode.string << encodeQueryAsString


encodeQueryAsString : Query -> String
encodeQueryAsString (Query query) =
    query
        |> List.map encodeField
        |> String.join "\n"
        |> surroundBraces


encodeField : Field -> String
encodeField (Field field) =
    field.id
        |> reverseAdd (addArguments field.arguments)
        |> reverseAdd (addSelectors field.selectors)


reverseAdd : String -> String -> String
reverseAdd =
    flip (++)


addSelectors : List Field -> String
addSelectors selectors =
    if List.isEmpty selectors then
        ""
    else
        selectors
            |> List.map encodeField
            |> String.join "\n"
            |> surroundBraces


addArguments : List ( String, String ) -> String
addArguments arguments =
    if List.isEmpty arguments then
        ""
    else
        arguments
            |> List.map toGraphQlArg
            |> String.join ", "
            |> surroundParen


toGraphQlArg : ( String, String ) -> String
toGraphQlArg ( param, value ) =
    param ++ ": " ++ value


decodeGraphQlQueries : Decode.Decoder a -> Decode.Decoder a
decodeGraphQlQueries =
    Decode.field "data"
