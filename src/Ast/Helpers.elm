module Ast.Helpers exposing (..)

import Combine exposing (..)
import Combine.Char exposing (..)
import String


type alias Name =
    String


type alias QualifiedType =
    List Name


type alias ModuleName =
    List String


type alias Alias =
    String


{-| Representation of Parser meta information
-}
type alias Meta =
    { line : Int
    , column : Int
    }


type alias Operator =
    ( String, Meta )


makeMeta : ParseLocation -> Meta
makeMeta { line, column } =
    { line = line
    , column =
        if column < 0 then
            0
        else
            column
    }


withMeta : Parser s (Meta -> b) -> Parser s b
withMeta p =
    withLocation (flip andMap p << succeed << makeMeta)


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
    let
        loName_ =
            name lower
                >>= (\n ->
                        if List.member n reserved then
                            fail <| "name '" ++ n ++ "' is reserved"
                        else
                            succeed n
                    )
    in
        string "_" <|> loName_


upName : Parser s String
upName =
    name upper


emptyTuple : Parser s String
emptyTuple =
    string "()"


operator : Parser s Operator
operator =
    lazy <|
        \() ->
            regex "[+\\-\\/*=.$<>:&|^?%#@~!]+|\x8As\x08"
                >>= (\n ->
                        if List.member n reservedOperators then
                            fail <| "operator '" ++ n ++ "' is reserved"
                        else
                            withLocation (\l -> succeed ( n, makeMeta l ))
                    )


functionName : Parser s String
functionName =
    loName


moduleName : Parser s ModuleName
moduleName =
    between_ spaces <| sepBy1 (string ".") upName
