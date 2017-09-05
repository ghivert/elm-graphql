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



type Value
  = Value
    { id : Maybe String
    , alias : Maybe String
    , arguments : List (String, String)
    , variables : List (String, String)
    , selectors : List Value
    }

new : Value
new =
  Value
    { id = Nothing
    , alias = Nothing
    , arguments = []
    , variables = []
    , selectors = []
    }

setId : String -> Value -> Value
setId id (Value value) =
  Value { value | id = Just id }

setAlias : String -> Value -> Value
setAlias alias (Value value) =
  Value { value | alias = Just alias }

unsetAlias : Value -> Value
unsetAlias (Value value) =
  Value { value | alias = Nothing }

setArguments : List (String, String) -> Value -> Value
setArguments arguments (Value value) =
  Value { value | arguments = arguments }

setVariables : List (String, String) -> Value -> Value
setVariables variables (Value value) =
  Value { value | variables = variables }

addSelectorsIn : Value -> List Value -> Value
addSelectorsIn (Value value) selectors =
  Value { value | selectors = List.append selectors value.selectors }

swapArgumentsAndVariables : Value -> Value
swapArgumentsAndVariables (Value value) =
  Value { value | arguments = value.variables }

addInValueArguments : Value -> (String, String) -> Value
addInValueArguments (Value value) arg =
  setArguments (arg :: value.arguments) (Value value)

addInValueVariables : Value -> (String, String) -> Value
addInValueVariables (Value value) var =
  setVariables (var :: value.variables) (Value value)



encodeValue : Value -> String
encodeValue value =
  value
    |> unsetAlias
    |> swapArgumentsAndVariables
    |> encodeValueHelp

encodeValueHelp : Value -> String
encodeValueHelp (Value value) =
  value.id
    |> Maybe.map (encodeName (Value value))
    |> Maybe.withDefault ""
    |> Helpers.reverseAdd (addSelectors value.selectors)

encodeName : Value -> String -> String
encodeName (Value value) id =
  addName value.alias ++ id
    |> Helpers.reverseAdd (addArguments value.arguments)

addName : Maybe String -> String
addName =
  Maybe.map (Helpers.reverseAdd ":") >> Maybe.withDefault ""

addSelectors : List Value -> String
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
      |> Helpers.join
      |> Helpers.betweenParen

joinGraphQlArgument : (String, String) -> String
joinGraphQlArgument (param, value) =
  param ++ ": " ++ value
