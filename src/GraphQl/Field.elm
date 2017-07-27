module GraphQl.Field
  exposing
    ( Field
    , new
    , setName, setArguments, setSelectors
    , addInFieldArgs
    , encodeField
    )

import Helpers

type Field
  = Field
    { id : String
    , name : Maybe String
    , arguments : List (String, String)
    , selectors : List Field
    }

new : String -> Field
new id =
  Field
    { id = id
    , name = Nothing
    , arguments = []
    , selectors = []
    }

setName : String -> Field -> Field
setName name (Field field) =
  Field { field | name = Just name }

setSelectors : List Field -> Field -> Field
setSelectors selectors (Field field) =
  Field { field | selectors = selectors }

setArguments : List (String, String) -> Field -> Field
setArguments arguments (Field field) =
  Field { field | arguments = arguments }

addInFieldArgs : Field -> (String, String) -> Field
addInFieldArgs (Field field) arg =
  setArguments (arg :: field.arguments) (Field field)

encodeField : Field -> String
encodeField (Field field) =
  field.id
    |> (++) (addName field.name)
    |> Helpers.reverseAdd (addArguments field.arguments)
    |> Helpers.reverseAdd (addSelectors field.selectors)

addName : Maybe String -> String
addName =
  Maybe.map (Helpers.reverseAdd ":") >> Maybe.withDefault ""

addSelectors : List Field -> String
addSelectors selectors =
  if List.isEmpty selectors then
    ""
  else
    selectors
      |> List.map encodeField
      |> String.join "\n"
      |> Helpers.surroundBraces

addArguments : List ( String, String ) -> String
addArguments arguments =
  if List.isEmpty arguments then
    ""
  else
    arguments
      |> List.map toGraphQlArg
      |> String.join ", "
      |> Helpers.surroundParen

toGraphQlArg : ( String, String ) -> String
toGraphQlArg ( param, value ) =
  param ++ ": " ++ value
