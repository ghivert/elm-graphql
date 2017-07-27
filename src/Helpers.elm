module Helpers exposing (..)

reverseAdd : String -> String -> String
reverseAdd =
  flip (++)

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

surroundNewline : String -> String
surroundNewline =
  surround "\n"
