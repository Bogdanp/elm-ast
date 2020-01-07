module Ast.Pattern exposing (applicationFromList, applicationToList, pattern)

import Ast.Common exposing (..)
import Ast.Helpers exposing (..)
import Ast.Literal exposing (..)
import Combine exposing (..)


{-| Parses a pattern for matching
-}
pattern : Parser s MPattern
pattern =
    lazy <|
        \() ->
            cycle
                [ asParser
                , consParser
                , appParser
                , terminalParser
                ]


{-| Parses an `as` keyword used to assign another name to an expression in pattern
-}
asParser : Parser s MPattern -> Parser s MPattern
asParser next =
    lazy <|
        \() ->
            choice
                [ (withMeta <| PAs <$> next <*> (symbol "as" *> varName)) >>= (asParser << succeed)
                , next
                ]


{-| Parses a cons (::) operator recursively
-}
consParser : Parser s MPattern -> Parser s MPattern
consParser next =
    lazy <|
        \() ->
            choice
                [ withMeta <|
                    PCons
                        <$> next
                        <* withMeta (symbol "::")
                        <*> consParser next
                , next
                ]


{-| Parses an application in a pattern. Used both for tagged union constructors
and for function definitions in let..in
-}
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


terminalParser : Parser s MPattern -> Parser s MPattern
terminalParser next =
    lazy <|
        \() ->
            choice
                [ wildcardParser
                , varParser
                , constructorParser
                , withMeta <| PLiteral <$> literalParser
                , withMeta <| PRecord <$> (braces <| commaSeparated_ <| withMeta loName)
                , withMeta <| PTuple <$> tupleParser next
                , withMeta <| PList <$> listParser next
                , parens next
                ]
{-| Parsers a distinguishable wildcard variable specified as an underscore -}
wildcardParser : Parser s MPattern
wildcardParser =
    withMeta <| always PWildcard <$> wildcard

{-| Parser a variable in a pattern -}
varParser : Parser s MPattern
varParser =
    withMeta <| PVariable <$> funName

{-| Parses a constructor function for a tagged union without its parameters -}
constructorParser : Parser s MPattern
constructorParser =
    withMeta <| PConstructor <$> upName


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
