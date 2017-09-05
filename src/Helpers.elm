module Helpers exposing (..)

reverseAdd : String -> String -> String
reverseAdd =
  flip (++)

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

join : List String -> String
join list =
  String.join ", " list
