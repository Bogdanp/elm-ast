module Ast.Helpers exposing
    ( between_
    , commaSeparated
    , commaSeparated_
    , countIndent
    , emptyTuple
    , exactIndentation
    , funName
    , functionName
    , initialSymbol
    , listParser
    , loName
    , logContent
    , moduleName
    , name
    , operator
    , optionalParens
    , reserved
    , reservedOperators
    , spaces
    , spacesOrIndentedNewline
    , spaces_
    , symbol
    , symbol_
    , tupleParser
    , upName
    , varName
    , wild
    )

import Ast.Common exposing (..)
import Combine exposing (..)
import Combine.Char exposing (..)
import Flip exposing (flip)
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


optionalParens : Parser s a -> Parser s a
optionalParens p =
    lazy <| \() -> or p (parens <| optionalParens p)


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
    regex "[^ \t\u{000D}\n]*" |> onerror "whitespace"


exactIndentation : Int -> Parser s String
exactIndentation int =
    regex ("\n*[ \\t]{" ++ String.fromInt int ++ "}\n*")


symbol_ : String -> Parser s String
symbol_ k =
    between_ whitespace (string k |> ignore (regex "( |\\n)+"))


symbol : String -> Parser s String
symbol k =
    between_ whitespace <| string k


initialSymbol : String -> Parser s String
initialSymbol k =
    string k |> ignore spaces_


commaSeparated : Parser s res -> Parser s (List res)
commaSeparated p =
    sepBy1 (string ",") <| between_ whitespace p


commaSeparated_ : Parser s res -> Parser s (List res)
commaSeparated_ p =
    sepBy (string ",") <| between_ whitespace p


name : Parser s Char -> Parser s String
name p =
    map String.cons p
        |> andMap (regex "[a-zA-Z0-9-_]*")


loName : Parser s String
loName =
    or wild varName


funName : Parser s String
funName =
    choice [ varName, parens operator ]


wild : Parser s String
wild =
    string "_"


varName : Parser s String
varName =
    name lower
        |> andThen
            (\n ->
                if List.member n reserved then
                    fail <| "name '" ++ n ++ "' is reserved"

                else
                    succeed n
            )


upName : Parser s String
upName =
    name upper


emptyTuple : Parser s String
emptyTuple =
    string "()"


operator : Parser s String
operator =
    lazy <|
        \() ->
            regex "[+\\-\\/*=.$<>:&|^?%#@~!]+|\u{008A}s\u{0008}"
                |> andThen
                    (\n ->
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
    xsParser |> andThen (Debug.log label >> succeed)


listParser : Parser s a -> Parser s (List a)
listParser el =
    brackets <| commaSeparated_ el


tupleParser : Parser s a -> Parser s (List a)
tupleParser el =
    parens (commaSeparated_ <| el)
        |> andThen
            (\a ->
                case a of
                    [ _ ] ->
                        fail "No single tuples"

                    anyOther ->
                        succeed anyOther
            )


spacesOrIndentedNewline : Int -> Parser s ()
spacesOrIndentedNewline indentation =
    lazy <|
        \() ->
            or (spaces_ |> keep (succeed ()))
                (countIndent
                    |> andThen
                        (\column ->
                            if column < indentation then
                                fail "Arguments have to be at least the same indentation as the function"

                            else
                                succeed ()
                        )
                )


countIndent : Parser s Int
countIndent =
    newline |> keep spaces |> andThen (String.filter (\char -> char == ' ') >> String.length >> succeed)
