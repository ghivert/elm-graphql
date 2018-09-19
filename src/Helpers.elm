module Helpers exposing (..)

reverseAdd : String -> String -> String
reverseAdd first second =
  second ++ first

between : String -> String -> String
between char string =
  char ++ string ++ char

betweenQuotes : String -> String
betweenQuotes =
  between "\""

betweenBraces : String -> String
betweenBraces string =
  "{" ++ string ++ "}"

betweenBrackets : String -> String
betweenBrackets string =
  "[" ++ string ++ "]"

betweenParen : String -> String
betweenParen string =
  "(" ++ string ++ ")"

betweenNewline : String -> String
betweenNewline =
  between "\n"
