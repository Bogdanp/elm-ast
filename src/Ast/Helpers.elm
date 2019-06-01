module Ast.Helpers exposing
    ( between_
    , commaSeparated
    , commaSeparated_
    , emptyTuple
    , exactIndentation
    , functionName
    , initialSymbol
    , loName
    , logContent
    , moduleName
    , name
    , operator
    , reserved
    , reservedOperators
    , spaces
    , spaces_
    , symbol
    , symbol_
    , upName
    , varName
    , wild
    )

import Ast.Common exposing (..)
import Combine exposing (..)
import Combine.Char exposing (..)
import String


reserved : List Name
reserved =
    [ "module"
    , "where"
    , "import"
    , "as"
    , "exposing"
    , "type"
    , "alias"
    , "port"
    , "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "case"
    , "of"
    ]


reservedOperators : List Name
reservedOperators =
    [ "=", ".", "..", "->", "--", "|", ":" ]


between_ : Parser s a -> Parser s res -> Parser s res
between_ p =
    between p p


spaces : Parser s String
spaces =
    regex "[ \\t]*"


spaces_ : Parser s String
spaces_ =
    regex "[ \\t]+"


notWhitespace_ : Parser s String
notWhitespace_ =
    regex "[^ \t\x0D\n]*" <?> "whitespace"


exactIndentation : Int -> Parser s String
exactIndentation int =
    regex ("\n*[ \\t]{" ++ toString int ++ "}\n*")


symbol_ : String -> Parser s String
symbol_ k =
    between_ whitespace (string k <* regex "( |\\n)+")


symbol : String -> Parser s String
symbol k =
    between_ whitespace <| string k


initialSymbol : String -> Parser s String
initialSymbol k =
    string k <* spaces_


commaSeparated : Parser s res -> Parser s (List res)
commaSeparated p =
    sepBy1 (string ",") <| between_ whitespace p


commaSeparated_ : Parser s res -> Parser s (List res)
commaSeparated_ p =
    sepBy (string ",") <| between_ whitespace p


name : Parser s Char -> Parser s String
name p =
    String.cons <$> p <*> regex "[a-zA-Z0-9-_]*"


loName : Parser s String
loName =
    wild <|> varName


upName : Parser s String
upName =
    name upper


wild : Parser s String
wild =
    string "_"


varName : Parser s String
varName =
    name lower
        >>= (\n ->
                if List.member n reserved then
                    fail <| "name '" ++ n ++ "' is reserved"

                else
                    succeed n
            )


emptyTuple : Parser s String
emptyTuple =
    string "()"


operator : Parser s String
operator =
    lazy <|
        \() ->
            regex "[+\\-\\/*=.$<>:&|^?%#@~!]+|\x8As\x08"
                >>= (\n ->
                        if List.member n reservedOperators then
                            fail <| "operator '" ++ n ++ "' is reserved"

                        else
                            succeed n
                    )


functionName : Parser s String
functionName =
    loName


moduleName : Parser s ModuleName
moduleName =
    between_ spaces <| sepBy1 (string ".") upName


logContent : String -> Parser s x -> Parser s x
logContent label xsParser =
    xsParser >>= (Debug.log label >> succeed)
