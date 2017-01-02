module Ast.Helpers exposing (..)

import Combine exposing (..)
import Combine.Char exposing (..)
import String

type alias Name = String
type alias QualifiedType = List Name
type alias ModuleName = List String
type alias Alias = String

reserved : List Name
reserved = [ "module", "where"
           , "import", "as", "exposing"
           , "type", "alias", "port"
           , "if", "then", "else"
           , "let", "in", "case", "of"
           ]

reservedOperators : List Name
reservedOperators =  [ "=", ".", "..", "->", "--", "|", ":" ]

between_ : Parser s a -> Parser s res -> Parser s res
between_ p = between p p

spaces : Parser s String
spaces = regex "[ \t]*"

spaces_ : Parser s String
spaces_ = regex "[ \t]+"

symbol : String -> Parser s String
symbol k =
  between_ whitespace (string k)

initialSymbol : String -> Parser s String
initialSymbol k =
  string k <* spaces

commaSeparated : Parser s res -> Parser s (List res)
commaSeparated p =
  sepBy1 (string ",") (between_ whitespace p)

commaSeparated_ : Parser s res -> Parser s (List res)
commaSeparated_ p =
  sepBy (string ",") (between_ whitespace p)

name : Parser s Char -> Parser s String
name p =
  String.cons <$> p <*> regex "[a-zA-Z0-9-_]*"

loName : Parser s String
loName =
  let
    loName_ =
      name lower |>
        andThen (\n ->
          if List.member n reserved
          then fail <| "name '" ++ n ++ "' is reserved"
          else succeed n)
  in
    string "_" <|> loName_

upName : Parser s String
upName = name upper

operator : Parser s String
operator =
  regex "[+-/*=.$<>:&|^?%#@~!]+" |>
    andThen (\n ->
      if List.member n reservedOperators
      then fail <| "operator '" ++ n ++ "' is reserved"
      else succeed n)

functionName : Parser s String
functionName = loName

moduleName : Parser s ModuleName
moduleName =
  between_ spaces <| sepBy1 (string ".") upName
