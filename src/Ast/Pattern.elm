module Ast.Pattern exposing (Pattern(..), applicationToList, pattern)

import Ast.Common exposing (..)
import Ast.Helpers exposing (..)
import Ast.Literal exposing (..)
import Combine exposing (..)


{-| Pattern to match
-}
type Pattern
    = PWild
    | PVariable Name
    | PConstructor Name
    | PLiteral Literal
    | PTuple (List Pattern)
    | PCons Pattern Pattern
    | PList (List Pattern)
    | PRecord (List Name)
    | PAs Pattern Name
    | PApplication Pattern Pattern


wildParser : Parser s Pattern
wildParser =
    always PWild <$> wild


varParser : Parser s Pattern
varParser =
    PVariable <$> funName


constructorParser : Parser s Pattern
constructorParser =
    PConstructor <$> upName



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
-- included helpers starting with other letters to enforce precedence and associativity
-- in case of left associativity, there is an unparsable left recursion, therefore I used
-- -- Elimination of left recursion:
-- -- A -> A a | b
-- -- ------------
-- -- A -> b A'
-- -- A' -> a A' | epsilon


{-| Parse a pattern
-}
pattern : Parser s Pattern
pattern =
    lazy <|
        \() ->
            tattern >>= pattern_


pattern_ : Pattern -> Parser s Pattern
pattern_ a =
    lazy <|
        \() ->
            ((PAs a <$> (symbol "as" *> varName)) >>= pattern_) <|> succeed a


tattern : Parser s Pattern
tattern =
    lazy <|
        \() ->
            (PCons <$> fattern <* symbol "::" <*> tattern)
                <|> fattern


fattern : Parser s Pattern
fattern =
    lazy <|
        \() ->
            withColumn
                (\column ->
                    chainl
                        ((\a b -> PApplication a b)
                            <$ spacesOrIndentedNewline (column + 1)
                        )
                        cattern
                )
                <|> cattern


cattern : Parser s Pattern
cattern =
    lazy <|
        \() ->
            choice
                [ wildParser
                , varParser
                , constructorParser
                , PLiteral <$> literalParser
                , PRecord <$> (braces <| commaSeparated_ loName)
                , PTuple <$> tupleParser pattern
                , PList <$> listParser pattern
                , parens pattern
                ]


applicationToList : Pattern -> List Pattern
applicationToList application =
    case application of
        PApplication left right ->
            applicationToList left ++ [ right ]

        other ->
            [ other ]
