module Ast.Helpers exposing (..)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import String

type alias Name = String
type alias QualifiedType = List Name
type alias ModuleName = List String
type alias Alias = String

sequence : List (Parser res) -> Parser (List res)
sequence ps =
  let
    accumulate acc ps cx =
      case ps of
        [] ->
          (Ok (List.reverse acc), cx)

        p::ps ->
          case app p cx of
            (Ok res, rcx) ->
              accumulate (res :: acc) ps rcx

            (Err ms, ecx) ->
              (Err ms, ecx)
  in
    primitive <| \cx ->
      accumulate [] ps cx

reserved : List Name
reserved = [ "module", "where"
           , "import", "as", "exposing"
           , "type", "alias", "port"
           , "if", "then", "else"
           , "let", "in", "case", "of"
           ]

reservedOperators : List Name
reservedOperators =  [ "=", ".", "..", "->", "--", "|", ":" ]

between' : Parser a -> Parser res -> Parser res
between' p = between p p

whitespace : Parser String
whitespace = regex "[ \r\t\n]*"

spaces : Parser String
spaces = regex "[ \t]*"

spaces' : Parser String
spaces' = regex "[ \t]+"

symbol : String -> Parser String
symbol k =
  between' whitespace (string k)

initialSymbol : String -> Parser String
initialSymbol k =
  string k <* spaces

commaSeparated : Parser res -> Parser (List res)
commaSeparated p =
  sepBy1 (string ",") (between' whitespace p)

commaSeparated' : Parser res -> Parser (List res)
commaSeparated' p =
  sepBy (string ",") (between' whitespace p)

name : Parser Char -> Parser String
name p =
  String.cons <$> p <*> regex "[a-zA-Z0-9-_']*"

loName : Parser String
loName =
  let
    loName' =
      name lower
        `andThen` \n ->
          if List.member n reserved
          then fail [ "name '" ++ n ++ "' is reserved" ]
          else succeed n
  in
    string "_" <|> loName'

upName : Parser String
upName = name upper

operator : Parser String
operator =
  between' (string "`") loName <|> symbolicOperator

symbolicOperator : Parser String
symbolicOperator =
  regex "[+-/*=.$<>:&|^?%#@~!]+"
    `andThen` \n ->
      if List.member n reservedOperators
      then fail [ "operator '" ++ n ++ "' is reserved" ]
      else succeed n

functionName : Parser String
functionName = loName

moduleName : Parser ModuleName
moduleName =
  between' spaces <| sepBy1 (string ".") upName
