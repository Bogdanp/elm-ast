module Ast.Helpers exposing
    ( between_
    , commaSeparated
    , commaSeparated_
    , countIndent
    , cycle
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
    , wildcard
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


optionalParens : Parser s a -> Parser s a
optionalParens p =
    lazy <| \() -> p <|> (parens <| optionalParens p)


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
    String.cons <$> p <*> regex "[a-zA-Z0-9_]*"


loName : Parser s String
loName =
    wildcard <|> varName


funName : Parser s String
funName =
    choice [ varName, parens operator ]


wildcard : Parser s String
wildcard =
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


listParser : Parser s a -> Parser s (List a)
listParser el =
    brackets (commaSeparated el <|> whitespace *> succeed [])


tupleParser : Parser s a -> Parser s (List a)
tupleParser el =
    parens (commaSeparated_ <| el)
        >>= (\a ->
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
            or (spaces_ *> succeed ())
                (countIndent
                    >>= (\column ->
                            if column < indentation then
                                fail "Arguments have to be at least the same indentation as the function"

                            else
                                succeed ()
                        )
                )


countIndent : Parser s Int
countIndent =
    newline *> spaces >>= (String.filter (\char -> char == ' ') >> String.length >> succeed)


{-| Cycle combinator executes the first parser passing the next parser as an argument
thanks to that the parser can utilize self recursion in the most lazy way possible

cycle [parserA, parserB, parserC] is equal to
parserA parserB
parserB parserC
parserC parserA

-}
cycle : List (Parser s a -> Parser s a) -> Parser s a
cycle parsers =
    lazy <|
        \() ->
            List.foldr identity (cycle parsers) parsers
