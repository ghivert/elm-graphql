module GraphQl.Field
  exposing
    ( Field
    , new
    , setId, setAlias, setArguments
    , addSelectorsIn
    , addInFieldArguments, addInFieldVariables
    , encodeField
    )

import Helpers



type Field a
  = Field
    { id : Maybe String
    , alias : Maybe String
    , arguments : List (String, String)
    , variables : List (String, String)
    , selectors : List (Field a)
    }

new : Field a
new =
  Field
    { id = Nothing
    , alias = Nothing
    , arguments = []
    , variables = []
    , selectors = []
    }

setId : String -> Field a -> Field a
setId id (Field value) =
  Field { value | id = Just id }

setAlias : String -> Field a -> Field a
setAlias alias (Field value) =
  Field { value | alias = Just alias }

unsetAlias : Field a -> Field a
unsetAlias (Field value) =
  Field { value | alias = Nothing }

setArguments : List (String, String) -> Field a -> Field a
setArguments arguments (Field value) =
  Field { value | arguments = arguments }

setVariables : List (String, String) -> Field a -> Field a
setVariables variables (Field value) =
  Field { value | variables = variables }

addSelectorsIn : Field a -> List (Field a) -> Field a
addSelectorsIn (Field value) selectors =
  Field { value | selectors = List.append selectors value.selectors }

swapArgumentsAndVariables : Field a -> Field a
swapArgumentsAndVariables (Field value) =
  Field { value | arguments = value.variables }

addInFieldArguments : Field a -> (String, String) -> Field a
addInFieldArguments (Field value) arg =
  setArguments (arg :: value.arguments) (Field value)

addInFieldVariables : Field a -> (String, String) -> Field a
addInFieldVariables (Field value) var =
  setVariables (var :: value.variables) (Field value)



encodeField : Field a -> String
encodeField value =
  value
    |> unsetAlias
    |> swapArgumentsAndVariables
    |> encodeFieldHelp

encodeFieldHelp : Field a -> String
encodeFieldHelp (Field value) =
  value.id
    |> Maybe.map (encodeName (Field value))
    |> Maybe.withDefault ""
    |> Helpers.reverseAdd (addSelectors value.selectors)

encodeName : Field a -> String -> String
encodeName (Field value) id =
  addName value.alias ++ id
    |> Helpers.reverseAdd (addArguments value.arguments)

addName : Maybe String -> String
addName =
  Maybe.map (Helpers.reverseAdd ":") >> Maybe.withDefault ""

addSelectors : List (Field a) -> String
addSelectors selectors =
  if List.isEmpty selectors then
    ""
  else
    selectors
      |> List.map encodeFieldHelp
      |> String.join "\n"
      |> Helpers.betweenNewline
      |> Helpers.betweenBraces

addArguments : List (String, String) -> String
addArguments arguments =
  if List.isEmpty arguments then
    ""
  else
    arguments
      |> List.map joinGraphQlArgument
      |> String.join ", "
      |> Helpers.betweenParen

joinGraphQlArgument : (String, String) -> String
joinGraphQlArgument (param, value) =
  param ++ ": " ++ value
