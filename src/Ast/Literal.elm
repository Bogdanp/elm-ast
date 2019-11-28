module Ast.Literal exposing (characterParser, floatParser, intParser, literalParser, stringParser)

import Ast.Common exposing (Literal(..))
import Ast.Helpers exposing (..)
import Char
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Hex


literalParser : Parser s Literal
literalParser =
    choice
        [ map Float floatParser
        , map Integer intParser
        , map Character characterParser
        , map String stringParser
        ]


characterParser : Parser s Char
characterParser =
    between_ (Combine.string "'")
        (or
            ((Combine.string "\\" |> keep (regex "(n|t|r|\\\\|x..)"))
                |> andThen
                    (\a ->
                        case String.uncons a of
                            Just ( 'n', "" ) ->
                                succeed '\n'

                            Just ( 't', "" ) ->
                                succeed '\t'

                            Just ( 'r', "" ) ->
                                succeed '\u{000D}'

                            Just ( '\\', "" ) ->
                                succeed '\\'

                            Just ( '0', "" ) ->
                                succeed '\u{0000}'

                            Just ( 'x', hex ) ->
                                hex
                                    |> String.toLower
                                    |> Hex.fromString
                                    |> Result.map Char.fromCode
                                    |> Result.map succeed
                                    |> Result.withDefault (fail "Invalid charcode")

                            Just other ->
                                fail ("No such character as \\" ++ String.fromChar (Tuple.first other))

                            Nothing ->
                                fail "No character"
                    )
            )
            anyChar
        )


stringParser : Parser s String
stringParser =
    let
        singleString =
            Combine.string "\""
                |> keep (regex "(\\\\\\\\|\\\\\"|[^\"\n])*")
                |> ignore (Combine.string "\"")

        multiString =
            Combine.string "\"\"\""
                |> keep (many (regex "[^\"]*"))
                |> ignore (Combine.string "\"\"\"")
                |> map String.concat
    in
    or multiString singleString


intParser : Parser s Int
intParser =
    Combine.Num.int


floatParser : Parser s Float
floatParser =
    Combine.Num.float
