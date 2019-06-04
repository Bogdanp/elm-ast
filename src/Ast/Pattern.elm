module Ast.Pattern exposing (Pattern(..), applicationFromList, applicationToList, pattern)

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
-- precedence describes which patterns' bind is stronger, in increasing order
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
    lazy <| \() -> List.foldr identity pattern precedence


precedence : List (Parser s Pattern -> Parser s Pattern)
precedence =
    [ asParser, consParser, appParser, terminalParser ]


asParser : Parser s Pattern -> Parser s Pattern
asParser next =
    lazy <| \() -> next >>= asParser_


asParser_ : Pattern -> Parser s Pattern
asParser_ a =
    lazy <|
        \() ->
            ((PAs a <$> (symbol "as" *> varName)) >>= asParser_) <|> succeed a


consParser : Parser s Pattern -> Parser s Pattern
consParser next =
    lazy <|
        \() ->
            (PCons <$> next <* symbol "::" <*> consParser next)
                <|> next


appParser : Parser s Pattern -> Parser s Pattern
appParser next =
    lazy <|
        \() ->
            withColumn
                (\column ->
                    chainl
                        ((\a b -> PApplication a b)
                            <$ spacesOrIndentedNewline (column + 1)
                        )
                        next
                )
                <|> next


terminalParser : Parser s Pattern -> Parser s Pattern
terminalParser next =
    lazy <|
        \() ->
            choice
                [ wildParser
                , varParser
                , constructorParser
                , PLiteral <$> literalParser
                , PRecord <$> (braces <| commaSeparated_ loName)
                , PTuple <$> tupleParser next
                , PList <$> listParser next
                , parens next
                ]


applicationToList : Pattern -> List Pattern
applicationToList application =
    case application of
        PApplication left right ->
            applicationToList left ++ [ right ]

        other ->
            [ other ]


applicationFromList : Pattern -> List Pattern -> Pattern
applicationFromList acc l =
    case l of
        right :: rest ->
            applicationFromList (PApplication acc right) rest

        _ ->
            acc
