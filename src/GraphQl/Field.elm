module GraphQl.Field
  exposing
    ( Field
    , new
    , setId, setName, setArguments
    , addSelectorsIn
    , addInFieldArgs, addInFieldVars
    , encodeField
    )

import Helpers

type Field
  = Field
    { id : Maybe String
    , name : Maybe String
    , arguments : List (String, String)
    , variables : List (String, String)
    , selectors : List Field
    }

new : Field
new =
  Field
    { id = Nothing
    , name = Nothing
    , arguments = []
    , variables = []
    , selectors = []
    }

setId : String -> Field -> Field
setId id (Field field) =
  Field { field | id = Just id }

setName : String -> Field -> Field
setName name (Field field) =
  Field { field | name = Just name }

setArguments : List (String, String) -> Field -> Field
setArguments arguments (Field field) =
  Field { field | arguments = arguments }

setVariables : List (String, String) -> Field -> Field
setVariables variables (Field field) =
  Field { field | variables = variables }

addSelectorsIn : Field -> List Field -> Field
addSelectorsIn (Field field) selectors =
  Field { field | selectors = selectors }

swapArgsAndVars : Field -> Field
swapArgsAndVars (Field field) =
  Field { field | arguments = field.variables }

addInFieldArgs : Field -> (String, String) -> Field
addInFieldArgs (Field field) arg =
  setArguments (arg :: field.arguments) (Field field)

addInFieldVars : Field -> (String, String) -> Field
addInFieldVars (Field field) var =
  setVariables (var :: field.variables) (Field field)


encodeField : Field -> String
encodeField (Field field) =
  Field { field | name = Nothing }
    |> swapArgsAndVars
    |> encodeFieldHelp

encodeFieldHelp : Field -> String
encodeFieldHelp (Field field) =
  field.id
    |> Maybe.map (encodeName field.name field.arguments)
    |> Maybe.withDefault ""
    |> Helpers.reverseAdd (addSelectors field.selectors)

encodeName : Maybe String -> List (String, String) -> String -> String
encodeName name arguments y =
  addName name ++ y
    |> Helpers.reverseAdd (addArguments arguments)

addName : Maybe String -> String
addName =
  Maybe.map (Helpers.reverseAdd ":") >> Maybe.withDefault ""

addSelectors : List Field -> String
addSelectors selectors =
  if List.isEmpty selectors then
    ""
  else
    selectors
      |> List.map encodeFieldHelp
      |> String.join "\n"
      |> Helpers.surroundNewline
      |> Helpers.surroundBraces

addArguments : List (String, String) -> String
addArguments arguments =
  if List.isEmpty arguments then
    ""
  else
    arguments
      |> List.map toGraphQlArg
      |> String.join ", "
      |> Helpers.surroundParen

toGraphQlArg : (String, String) -> String
toGraphQlArg (param, value) =
  param ++ ": " ++ value
