module Ast.Pattern exposing (applicationFromList, applicationToList, pattern)

import Ast.Common exposing (..)
import Ast.Helpers exposing (..)
import Ast.Literal exposing (..)
import Combine exposing (..)


wildParser : Parser s MPattern
wildParser =
    withMeta <| always PWild <$> wild


varParser : Parser s MPattern
varParser =
    withMeta <| PVariable <$> funName


constructorParser : Parser s MPattern
constructorParser =
    withMeta <| PConstructor <$> upName



-- Our grammar is
-- pattern -> terminal | constructor | tuple | list | cons | as | (pattern)
-- terminal -> wild | var | literal | record
-- constructor -> name constructorPatterns
-- constructorPatterns -> pattern constructorPatterns | epsilon
-- tuple -> (tupleElems)
-- tupleElems -> pattern, tupleElems | epsilon
-- list -> [listElems]
-- listElems -> pattern, listElems | epsilon
-- cons -> pattern :: pattern
-- as -> pattern as varName
-- precedence describes which patterns' bind is stronger, in increasing order
-- in case of left associativity, there is an unparsable left recursion, therefore I used
-- -- Elimination of left recursion:
-- -- A -> A a | b
-- -- ------------
-- -- A -> b A'
-- -- A' -> a A' | epsilon


{-| Parse a pattern
-}
pattern : Parser s MPattern
pattern =
    lazy <| \() -> List.foldr identity pattern precedence


precedence : List (Parser s MPattern -> Parser s MPattern)
precedence =
    [ asParser, consParser, appParser, terminalParser ]


asParser : Parser s MPattern -> Parser s MPattern
asParser next =
    lazy <| \() -> next >>= asParser_


asParser_ : MPattern -> Parser s MPattern
asParser_ a =
    lazy <|
        \() ->
            ((withMeta <| PAs a <$> (symbol "as" *> varName)) >>= asParser_)
                <|> succeed a


consParser : Parser s MPattern -> Parser s MPattern
consParser next =
    lazy <|
        \() ->
            (withMeta <|
                PCons
                    <$> next
                    <* withMeta (symbol "::")
                    <*> consParser next
            )
                <|> next


appParser : Parser s MPattern -> Parser s MPattern
appParser next =
    lazy <|
        \() ->
            withLocation
                (\l ->
                    chainl
                        ((\a b ->
                            addMeta l.line l.column <|
                                PApplication a b
                         )
                            <$ spacesOrIndentedNewline (l.column + 1)
                        )
                        next
                )
                <|> next


terminalParser : Parser s MPattern -> Parser s MPattern
terminalParser next =
    lazy <|
        \() ->
            choice
                [ wildParser
                , varParser
                , constructorParser
                , withMeta <| PLiteral <$> literalParser
                , withMeta <| PRecord <$> (braces <| commaSeparated_ <| withMeta loName)
                , withMeta <| PTuple <$> tupleParser next
                , withMeta <| PList <$> listParser next
                , parens next
                ]


applicationToList : MPattern -> List MPattern
applicationToList application =
    case application of
        ( PApplication left right, _ ) ->
            applicationToList left ++ [ right ]

        other ->
            [ other ]


applicationFromList : MPattern -> List MPattern -> MPattern
applicationFromList acc l =
    case l of
        (( _, meta ) as right) :: rest ->
            applicationFromList ( PApplication acc right, meta ) rest

        _ ->
            acc
