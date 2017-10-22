module GraphQl.Value
  exposing
    ( Value
    , new
    , setId, setAlias, setArguments
    , addSelectorsIn
    , addInValueArguments, addInValueVariables
    , encodeValue
    )

import Helpers



type Value a
  = Value
    { id : Maybe String
    , alias : Maybe String
    , arguments : List (String, String)
    , variables : List (String, String)
    , selectors : List (Value a)
    }

new : Value a
new =
  Value
    { id = Nothing
    , alias = Nothing
    , arguments = []
    , variables = []
    , selectors = []
    }

setId : String -> Value a -> Value a
setId id (Value value) =
  Value { value | id = Just id }

setAlias : String -> Value a -> Value a
setAlias alias (Value value) =
  Value { value | alias = Just alias }

unsetAlias : Value a -> Value a
unsetAlias (Value value) =
  Value { value | alias = Nothing }

setArguments : List (String, String) -> Value a -> Value a
setArguments arguments (Value value) =
  Value { value | arguments = arguments }

setVariables : List (String, String) -> Value a -> Value a
setVariables variables (Value value) =
  Value { value | variables = variables }

addSelectorsIn : Value a -> List (Value a) -> Value a
addSelectorsIn (Value value) selectors =
  Value { value | selectors = List.append selectors value.selectors }

swapArgumentsAndVariables : Value a -> Value a
swapArgumentsAndVariables (Value value) =
  Value { value | arguments = value.variables }

addInValueArguments : Value a -> (String, String) -> Value a
addInValueArguments (Value value) arg =
  setArguments (arg :: value.arguments) (Value value)

addInValueVariables : Value a -> (String, String) -> Value a
addInValueVariables (Value value) var =
  setVariables (var :: value.variables) (Value value)



encodeValue : Value a -> String
encodeValue value =
  value
    |> unsetAlias
    |> swapArgumentsAndVariables
    |> encodeValueHelp

encodeValueHelp : Value a -> String
encodeValueHelp (Value value) =
  value.id
    |> Maybe.map (encodeName (Value value))
    |> Maybe.withDefault ""
    |> Helpers.reverseAdd (addSelectors value.selectors)

encodeName : Value a -> String -> String
encodeName (Value value) id =
  addName value.alias ++ id
    |> Helpers.reverseAdd (addArguments value.arguments)

addName : Maybe String -> String
addName =
  Maybe.map (Helpers.reverseAdd ":") >> Maybe.withDefault ""

addSelectors : List (Value a) -> String
addSelectors selectors =
  if List.isEmpty selectors then
    ""
  else
    selectors
      |> List.map encodeValueHelp
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
