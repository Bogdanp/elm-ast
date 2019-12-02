module Ast.Expression exposing
    ( Expression(..), MExp
    , expression
    , term
    )

{-| This module exposes parsers for Elm expressions.


# Types

@docs Expression, MExp


# rs

@docs expression


# Expression

@docs term

-}

import Ast.BinOp exposing (..)
import Ast.Common exposing (..)
import Ast.Helpers exposing (..)
import Ast.Literal exposing (..)
import Ast.Pattern exposing (..)
import Combine exposing (..)
import Dict exposing (Dict)
import List exposing (singleton)
import List.Extra exposing (break)
import String


type Collect a
    = Cont a
    | Stop a


{-| Representation for Elm's expression containing it's meta information
-}
type alias MExp =
    WithMeta Expression {}


type alias Operator =
    WithMeta String {}


{-| Representations for Elm's expressions.
-}
type Expression
    = Literal Literal
    | Variable Name
    | Constructor Name
    | External (List Name) MExp
    | List (List MExp)
    | Tuple (List MExp)
    | Access MExp (List MName)
    | AccessFunction Name
    | Record (List ( MName, MExp ))
    | RecordUpdate MName (List ( MName, MExp ))
    | If MExp MExp MExp
    | Let (List ( MPattern, MExp )) MExp
    | Case MExp (List ( MPattern, MExp ))
    | Lambda (List MPattern) MExp
    | Application MExp MExp
    | BinOp MExp MExp MExp


character : Parser s MExp
character =
    withMeta <| map (Literal << Character) characterParser


string : Parser s MExp
string =
    withMeta <| map (Literal << String) stringParser


integer : Parser s MExp
integer =
    withMeta <| map (Literal << Integer) intParser


float : Parser s MExp
float =
    withMeta <| map (Literal << Float) floatParser


access : Parser s MExp
access =
    map Access variable
        |> andMap (many1 (Combine.string "." |> keep (withMeta varName)))
        |> withMeta


accessFunction : Parser s MExp
accessFunction =
    map AccessFunction (Combine.string "." |> keep loName)
        |> withMeta


external : Parser s MExp
external =
    map External (many1 (upName |> ignore (Combine.string ".")))
        |> andMap (or variable constructor)
        |> withMeta


variable : Parser s MExp
variable =
    map Variable
        (choice
            [ loName
            , parens operator
            , parens (Combine.regex ",+")
            , emptyTuple
            ]
        )
        |> withMeta


constructor : Parser s MExp
constructor =
    withMeta <| map Constructor upName


list : OpTable -> Parser s MExp
list ops =
    lazy <|
        \() ->
            map List (listParser <| expression ops)
                |> withMeta


tuple : OpTable -> Parser s MExp
tuple ops =
    lazy <|
        \() ->
            map Tuple (tupleParser (expression ops))
                |> withMeta


record : OpTable -> Parser s MExp
record ops =
    lazy <|
        \() ->
            withMeta <|
                map Record <|
                    braces <|
                        commaSeparated <|
                            (map Tuple.pair (withMeta loName)
                                |> andMap (symbol "=" |> keep (expression ops))
                            )


simplifiedRecord : Parser s MExp
simplifiedRecord =
    let
        branch { line, column } =
            loName
                |> map
                    (\a ->
                        ( addMeta line column a
                        , addMeta line column (Variable a)
                        )
                    )
    in
    lazy <|
        \() ->
            withMeta <| map Record <| braces (commaSeparated (withLocation branch))


recordUpdate : OpTable -> Parser s MExp
recordUpdate ops =
    lazy <|
        \() ->
            map RecordUpdate (symbol "{" |> keep (withMeta loName))
                |> andMap
                    (symbol "|"
                        |> keep
                            (commaSeparated
                                (map Tuple.pair (withMeta loName)
                                    |> andMap (symbol "=" |> keep (expression ops))
                                )
                            )
                        |> ignore (Combine.string "}")
                    )
                |> withMeta


letExpression : OpTable -> Parser s MExp
letExpression ops =
    lazy <|
        \() ->
            map Let (symbol_ "let" |> keep (many1 (letBinding ops)))
                |> andMap (symbol "in" |> keep (expression ops))
                |> withMeta


letBinding : OpTable -> Parser s ( MPattern, MExp )
letBinding ops =
    lazy <|
        \() ->
            map Tuple.pair (between_ whitespace pattern)
                |> andMap (symbol "=" |> keep (expression ops))


ifExpression : OpTable -> Parser s MExp
ifExpression ops =
    lazy <|
        \() ->
            map If (symbol "if" |> keep (expression ops))
                |> andMap (symbol "then" |> keep (expression ops))
                |> andMap (symbol "else" |> keep (expression ops))
                |> withMeta


caseExpression : OpTable -> Parser s MExp
caseExpression ops =
    let
        binding indent =
            lazy <|
                \() ->
                    map Tuple.pair (exactIndentation indent |> keep pattern)
                        |> andMap (symbol "->" |> keep (expression ops))
    in
    lazy <|
        \() ->
            map Case (symbol "case" |> keep (expression ops))
                |> andMap
                    (whitespace
                        |> keep (Combine.string "of")
                        |> keep (lookAhead countIndent)
                        |> andThen
                            (\indent ->
                                many1 (binding indent)
                            )
                    )
                |> withMeta


lambda : OpTable -> Parser s MExp
lambda ops =
    lazy <|
        \() ->
            map Lambda (symbol "\\" |> keep (pattern |> andThen (succeed << applicationToList)))
                |> andMap (symbol "->" |> keep (expression ops))
                |> withMeta


application : OpTable -> Parser s MExp
application ops =
    lazy <|
        \() ->
            withLocation
                (\l ->
                    chainl
                        (onsuccess
                            (\a b -> addMeta l.line l.column (Application a b))
                            (spacesOrIndentedNewline (l.column + 1))
                        )
                        (term ops)
                )


successOrEmptyList : Parser s (List a) -> Parser s (List a)
successOrEmptyList p =
    lazy <|
        \() ->
            choice [ p, succeed [] ]


next_ : OpTable -> Parser s (List ( WithMeta String {}, MExp ))
next_ ops =
    (withMeta <| between_ whitespace <| operator)
        |> andThen
            (\o ->
                or (map Cont (application ops)) (map Stop (expression ops))
                    |> andThen
                        (\e ->
                            case e of
                                Cont t ->
                                    map ((::) ( o, t )) (successOrEmptyList (next_ ops))

                                Stop ex ->
                                    succeed [ ( o, ex ) ]
                        )
            )


binary : OpTable -> Parser s MExp
binary ops =
    application ops
        |> andThen
            (\e ->
                successOrEmptyList (next_ ops)
                    |> andThen (\eops -> split ops 0 e eops)
            )


{-| A parser for term
-}
term : OpTable -> Parser s MExp
term ops =
    lazy <|
        \() ->
            choice
                [ external
                , access
                , variable
                , constructor
                , accessFunction
                , string
                , float
                , integer
                , character
                , parens (between_ whitespace (expression ops))
                , list ops
                , tuple ops
                , recordUpdate ops
                , record ops
                , simplifiedRecord
                ]


{-| A parser for Elm expressions.
-}
expression : OpTable -> Parser s MExp
expression ops =
    lazy <|
        \() ->
            choice
                [ binary ops
                , letExpression ops
                , caseExpression ops
                , ifExpression ops
                , lambda ops
                ]


op : OpTable -> String -> ( Assoc, Int )
op ops n =
    Dict.get n ops
        |> Maybe.withDefault ( L, 9 )


assoc : OpTable -> String -> Assoc
assoc ops n =
    Tuple.first <| op ops n


level : OpTable -> String -> Int
level ops n =
    Tuple.second <| op ops n


hasLevel : OpTable -> Int -> ( Operator, MExp ) -> Bool
hasLevel ops l ( n, _ ) =
    level ops (dropMeta n) == l


split : OpTable -> Int -> MExp -> List ( Operator, MExp ) -> Parser s MExp
split ops l e eops =
    case eops of
        [] ->
            succeed e

        _ ->
            findAssoc ops l eops
                |> andThen
                    (\association ->
                        sequence (splitLevel ops l e eops)
                            |> andThen
                                (\es ->
                                    let
                                        ops_ =
                                            List.filterMap
                                                (\x ->
                                                    if hasLevel ops l x then
                                                        Just (Tuple.first x)

                                                    else
                                                        Nothing
                                                )
                                                eops
                                    in
                                    case association of
                                        R ->
                                            joinR es ops_

                                        _ ->
                                            joinL es ops_
                                )
                    )


splitLevel : OpTable -> Int -> MExp -> List ( Operator, MExp ) -> List (Parser s MExp)
splitLevel ops l e eops =
    case break (hasLevel ops l) eops of
        ( lops, ( _, e_ ) :: rops ) ->
            split ops (l + 1) e lops :: splitLevel ops l e_ rops

        ( lops, [] ) ->
            [ split ops (l + 1) e lops ]


joinL : List MExp -> List Operator -> Parser s MExp
joinL es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, ( e, { line, column } ) :: remO ) ->
            joinL
                (addMeta line
                    column
                    (BinOp (addMeta line column <| Variable e) a b)
                    :: remE
                )
                remO

        _ ->
            fail ""


joinR : List MExp -> List Operator -> Parser s MExp
joinR es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, ( e, { line, column } ) :: remO ) ->
            joinR (b :: remE) remO
                |> andThen
                    (\exp ->
                        succeed
                            (addMeta line column <|
                                BinOp (addMeta line column (Variable e)) a exp
                            )
                    )

        _ ->
            fail ""


findAssoc : OpTable -> Int -> List ( Operator, MExp ) -> Parser s Assoc
findAssoc ops l eops =
    let
        lops =
            List.filter (hasLevel ops l) eops

        assocs =
            List.map (assoc ops << dropMeta << Tuple.first) lops

        error issue =
            let
                operators =
                    List.map (Tuple.first >> dropMeta) lops |> String.join " and "
            in
            "conflicting " ++ issue ++ " for operators " ++ operators
    in
    if List.all ((==) L) assocs then
        succeed L

    else if List.all ((==) R) assocs then
        succeed R

    else if List.all ((==) N) assocs then
        case assocs of
            [ _ ] ->
                succeed N

            _ ->
                fail <| error "precedence"

    else
        fail <| error "associativity"
