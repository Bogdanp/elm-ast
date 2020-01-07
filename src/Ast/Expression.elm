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
    withMeta <|
        Literal
            << Character
            <$> characterParser


string : Parser s MExp
string =
    withMeta <|
        (Literal << String <$> stringParser)


integer : Parser s MExp
integer =
    withMeta <| Literal << Integer <$> intParser


float : Parser s MExp
float =
    withMeta <| Literal << Float <$> floatParser


access : Parser s MExp
access =
    withMeta <| Access <$> variable <*> many1 (Combine.string "." *> withMeta varName)


accessFunction : Parser s MExp
accessFunction =
    withMeta <| AccessFunction <$> (Combine.string "." *> loName)


external : Parser s MExp
external =
    withMeta <| External <$> many1 (upName <* Combine.string ".") <*> (variable <|> constructor)


variable : Parser s MExp
variable =
    withMeta <|
        Variable
            <$> choice
                    [ loName
                    , parens operator
                    , parens (Combine.regex ",+")
                    , emptyTuple
                    ]


constructor : Parser s MExp
constructor =
    withMeta <| Constructor <$> upName


list : OpTable -> Parser s MExp
list ops =
    lazy <|
        \() ->
            withMeta <| List <$> (listParser <| expression ops)


tuple : OpTable -> Parser s MExp
tuple ops =
    lazy <|
        \() ->
            withMeta <|
                Tuple
                    <$> tupleParser (expression ops)


record : OpTable -> Parser s MExp
record ops =
    lazy <|
        \() ->
            withMeta <|
                Record
                    <$> braces
                            (commaSeparated
                                ((,)
                                    <$> withMeta loName
                                    <*> (symbol "=" *> expression ops)
                                )
                                <|> (whitespace *> succeed [])
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
            withMeta <|
                Record
                    <$> braces (commaSeparated (withLocation branch))


recordUpdate : OpTable -> Parser s MExp
recordUpdate ops =
    lazy <|
        \() ->
            withMeta <|
                RecordUpdate
                    <$> (symbol "{" *> withMeta loName)
                    <*> (symbol "|"
                            *> commaSeparated ((,) <$> withMeta loName <*> (symbol "=" *> expression ops))
                            <* Combine.string "}"
                        )


letExpression : OpTable -> Parser s MExp
letExpression ops =
    lazy <|
        \() ->
            withMeta <|
                Let
                    <$> (symbol_ "let" *> many1 (letBinding ops))
                    <*> (symbol "in" *> expression ops)


letBinding : OpTable -> Parser s ( MPattern, MExp )
letBinding ops =
    lazy <|
        \() ->
            (,)
                <$> between_ whitespace pattern
                <*> (symbol "=" *> expression ops)


ifExpression : OpTable -> Parser s MExp
ifExpression ops =
    lazy <|
        \() ->
            withMeta <|
                If
                    <$> (symbol "if" *> expression ops)
                    <*> (symbol "then" *> expression ops)
                    <*> (symbol "else" *> expression ops)


caseExpression : OpTable -> Parser s MExp
caseExpression ops =
    let
        binding indent =
            lazy <|
                \() ->
                    (,)
                        <$> (exactIndentation indent *> pattern)
                        <*> (symbol "->" *> expression ops)
    in
    lazy <|
        \() ->
            withMeta <|
                Case
                    <$> (symbol "case" *> expression ops)
                    <*> (whitespace
                            *> Combine.string "of"
                            *> lookAhead countIndent
                            >>= (\indent ->
                                    many1 (binding indent)
                                )
                        )


lambda : OpTable -> Parser s MExp
lambda ops =
    lazy <|
        \() ->
            withMeta <|
                Lambda
                    <$> (symbol "\\" *> (pattern >>= succeed << applicationToList))
                    <*> (symbol "->" *> expression ops)


application : OpTable -> Parser s MExp
application ops =
    lazy <|
        \() ->
            withLocation
                (\l ->
                    chainl
                        ((\a b -> addMeta l.line l.column (Application a b)) <$ spacesOrIndentedNewline (l.column + 1))
                        (term ops)
                )


successOrEmptyList : Parser s (List a) -> Parser s (List a)
successOrEmptyList p =
    lazy <| \() -> choice [ p, succeed [] ]


unary : OpTable -> Parser s MExp
unary ops =
    -- support only (-), as far as I can tell there are no other unary operators
    withMeta <|
        Application
            <$> (withMeta <| Variable <$> Combine.string "-")
            <*> variable


binary : OpTable -> Parser s MExp
binary ops =
    lazy <|
        \() ->
            let
                next =
                    (withMeta <| between_ whitespace <| operator)
                        >>= (\op ->
                                lazy <|
                                    \() ->
                                        or (Cont <$> application ops) (Stop <$> expression ops)
                                            >>= (\e ->
                                                    case e of
                                                        Cont t ->
                                                            (::) ( op, t ) <$> successOrEmptyList next

                                                        Stop ex ->
                                                            succeed [ ( op, ex ) ]
                                                )
                            )
            in
            application ops
                >>= (\e -> successOrEmptyList next >>= (\eops -> split ops 0 e eops))


{-| A parses for term
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
                , unary ops
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
                >>= (\assoc ->
                        sequence (splitLevel ops l e eops)
                            >>= (\es ->
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
                                    case assoc of
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
