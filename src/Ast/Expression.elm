module Ast.Expression
    exposing
        ( Expression(..)
        , expression
        , term
        )

{-| This module exposes parsers for Elm expressions.


# Types

@docs Expression


# Parsers

@docs expression


# Expression

@docs term

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Dict exposing (Dict)
import List.Extra exposing (break, singleton)
import String
import Ast.BinOp exposing (..)
import Ast.Helpers exposing (..)
import Hex
import Char


type Collect a
    = Cont a
    | Stop a


{-| Representations for Elm's expressions.
-}
type Expression
    = Character Char Meta
    | String String Meta
    | Integer Int Meta
    | Float Float Meta
    | List (List Expression) Meta
    | Tuple (List Expression) Meta
    | Access Expression (List Name) Meta
    | AccessFunction Name Meta
    | Record (List ( Name, Expression )) Meta
    | RecordUpdate Name (List ( Name, Expression )) Meta
    | If Expression Expression Expression Meta
    | Let (List ( Expression, Expression )) Expression Meta
    | Case Expression (List ( Expression, Expression )) Meta
    | Lambda (List Expression) Expression Meta
    | Variable (List Name) Meta
    | Application Expression Expression Meta
    | BinOp Expression Expression Expression Meta


character : Parser s Expression
character =
    withMeta <|
        Character
            <$> between_ (Combine.string "'")
                    (((Combine.string "\\" *> regex "(n|t|r|\\\\|x..)")
                        >>= (\a ->
                                case String.uncons a of
                                    Just ( 'n', "" ) ->
                                        succeed '\n'

                                    Just ( 't', "" ) ->
                                        succeed '\t'

                                    Just ( 'r', "" ) ->
                                        succeed '\x0D'

                                    Just ( '\\', "" ) ->
                                        succeed '\\'

                                    Just ( '0', "" ) ->
                                        succeed '\x00'

                                    Just ( 'x', hex ) ->
                                        hex
                                            |> String.toLower
                                            |> Hex.fromString
                                            |> Result.map Char.fromCode
                                            |> Result.map succeed
                                            |> Result.withDefault (fail "Invalid charcode")

                                    Just other ->
                                        fail ("No such character as \\" ++ toString other)

                                    Nothing ->
                                        fail "No character"
                            )
                     )
                        <|> anyChar
                    )


string : Parser s Expression
string =
    let
        singleString =
            String
                <$> (Combine.string "\"" *> regex "(\\\\\\\\|\\\\\"|[^\"\n])*" <* Combine.string "\"")

        multiString =
            (String << String.concat)
                <$> (Combine.string "\"\"\"" *> many (regex "[^\"]*") <* Combine.string "\"\"\"")
    in
        withMeta <| multiString <|> singleString


integer : Parser s Expression
integer =
    withMeta <| Integer <$> Combine.Num.int


float : Parser s Expression
float =
    withMeta <| Float <$> Combine.Num.float


access : Parser s Expression
access =
    withMeta <| Access <$> variable <*> many1 (Combine.string "." *> loName)


accessFunction : Parser s Expression
accessFunction =
    withMeta <| AccessFunction <$> (Combine.string "." *> loName)


variable : Parser s Expression
variable =
    withMeta <|
        Variable
            <$> choice
                    [ singleton <$> loName
                    , sepBy1 (Combine.string ".") upName
                    , singleton <$> parens (Combine.regex ",+")
                    , singleton <$> emptyTuple
                    ]


list : OpTable -> Parser s Expression
list ops =
    lazy <|
        \() ->
            withMeta <| List <$> brackets (commaSeparated_ <| expression ops)


tuple : OpTable -> Parser s Expression
tuple ops =
    lazy <|
        \() ->
            withMeta <|
                Tuple
                    <$> (parens (commaSeparated_ <| expression ops)
                            >>= \a ->
                                    case a of
                                        [ _ ] ->
                                            fail "No single tuples"

                                        anyOther ->
                                            succeed anyOther
                        )


record : OpTable -> Parser s Expression
record ops =
    lazy <|
        \() ->
            withMeta <|
                Record
                    <$> braces
                            (commaSeparated
                                ((,)
                                    <$> loName
                                    <*> (symbol "=" *> expression ops)
                                )
                            )


simplifiedRecord : Parser s Expression
simplifiedRecord =
    lazy <|
        \() ->
            withMeta <|
                Record
                    <$> (braces
                            (commaSeparated
                                (withMeta
                                    ((\a -> (\m -> ( a, Variable [ a ] m )))
                                        <$> loName
                                    )
                                )
                            )
                        )


recordUpdate : OpTable -> Parser s Expression
recordUpdate ops =
    lazy <|
        \() ->
            withMeta <|
                RecordUpdate
                    <$> (symbol "{" *> loName)
                    <*> (symbol "|"
                            *> (commaSeparated ((,) <$> loName <*> (symbol "=" *> expression ops)))
                            <* symbol "}"
                        )


letExpression : OpTable -> Parser s Expression
letExpression ops =
    lazy <|
        \() ->
            withMeta <|
                Let
                    <$> (symbol_ "let" *> many1 (letBinding ops))
                    <*> (symbol "in" *> expression ops)


letBinding : OpTable -> Parser s ( Expression, Expression )
letBinding ops =
    lazy <|
        \() ->
            (,)
                <$> (between_ whitespace <| expression ops)
                <*> (symbol "=" *> expression ops)


ifExpression : OpTable -> Parser s Expression
ifExpression ops =
    lazy <|
        \() ->
            withMeta <|
                If
                    <$> (symbol "if" *> expression ops)
                    <*> (symbol "then" *> expression ops)
                    <*> (symbol "else" *> expression ops)


caseExpression : OpTable -> Parser s Expression
caseExpression ops =
    lazy <|
        \() ->
            withMeta <|
                Case
                    <$> (symbol "case" *> expression ops)
                    <*> (symbol "of" *> many1 (caseBinding ops))


caseBinding : OpTable -> Parser s ( Expression, Expression )
caseBinding ops =
    lazy <|
        \() ->
            (,)
                <$> (whitespace *> expression ops)
                <*> (symbol "->" *> expression ops)


lambda : OpTable -> Parser s Expression
lambda ops =
    lazy <|
        \() ->
            withMeta <|
                Lambda
                    <$> (symbol "\\" *> many (between_ spaces <| term ops))
                    <*> (symbol "->" *> expression ops)


application : OpTable -> Parser s Expression
application ops =
    let
        flippedApp m e1 e2 =
            Application e1 e2 m
    in
        lazy <|
            \() ->
                chainl
                    (withMeta ((\m -> flippedApp m) <$ spacesOrIndentedNewline ops))
                    (term ops)


negate : Maybe a -> Parser s String
negate x =
    case x of
        Just _ ->
            -- next line starts a new case or let binding
            fail ""

        Nothing ->
            succeed ""


maybeBindingAhead : OpTable -> Parser s String
maybeBindingAhead ops =
    lazy <|
        \() ->
            lookAhead <|
                (maybe << choice) [ letBinding ops, caseBinding ops ]
                    >>= negate


spacesOrIndentedNewline : OpTable -> Parser s String
spacesOrIndentedNewline ops =
    lazy <|
        \() ->
            (regex "[ \\t]*\n[ \\t]+" *> maybeBindingAhead ops) <|> spaces_


operatorOrAsBetween : Parser s Operator
operatorOrAsBetween =
    lazy <|
        \() ->
            between_ whitespace <| operator <|> withMeta ((,) <$> symbol_ "as")


binary : OpTable -> Parser s Expression
binary ops =
    lazy <|
        \() ->
            let
                next =
                    operatorOrAsBetween
                        >>= \op ->
                                lazy <|
                                    \() ->
                                        (or (Cont <$> application ops) (Stop <$> expression ops))
                                            >>= \e ->
                                                    case e of
                                                        Cont t ->
                                                            ((::) ( op, t )) <$> collect

                                                        Stop ex ->
                                                            succeed [ ( op, ex ) ]

                collect =
                    lazy <| \() -> choice [ next, succeed [] ]
            in
                application ops
                    >>= (\e -> collect >>= \eops -> split ops 0 e eops)


{-| A parses for term
-}
term : OpTable -> Parser s Expression
term ops =
    lazy <|
        \() ->
            choice
                [ access
                , variable
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
expression : OpTable -> Parser s Expression
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


hasLevel : OpTable -> Int -> String -> Bool
hasLevel ops l n =
    level ops n == l


split : OpTable -> Int -> Expression -> List ( Operator, Expression ) -> Parser s Expression
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
                                                (\( x, _ ) ->
                                                    if hasLevel ops l (Tuple.first x) then
                                                        Just x
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


splitLevel : OpTable -> Int -> Expression -> List ( Operator, Expression ) -> List (Parser s Expression)
splitLevel ops l e eops =
    case break ((hasLevel ops l) << Tuple.first << Tuple.first) eops of
        ( lops, ( _, e_ ) :: rops ) ->
            split ops (l + 1) e lops :: splitLevel ops l e_ rops

        ( lops, [] ) ->
            [ split ops (l + 1) e lops ]


joinL : List Expression -> List Operator -> Parser s Expression
joinL es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, ( op, m ) :: remO ) ->
            joinL
                ((BinOp
                    (Variable [ op ] m)
                    a
                    b
                    m
                 )
                    :: remE
                )
                remO

        _ ->
            fail ""


joinR : List Expression -> List Operator -> Parser s Expression
joinR es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, ( op, m ) :: remO ) ->
            joinR (b :: remE) remO
                >>= (\e ->
                        succeed
                            (BinOp
                                (Variable [ op ] m)
                                a
                                e
                                m
                            )
                    )

        _ ->
            fail ""


findAssoc : OpTable -> Int -> List ( Operator, Expression ) -> Parser s Assoc
findAssoc ops l eops =
    let
        bareops =
            List.map (Tuple.first << Tuple.first) eops

        lops =
            List.filter (hasLevel ops l) bareops

        assocs =
            List.map (assoc ops) bareops

        error issue =
            "conflicting "
                ++ issue
                ++ " for operators "
                ++ (bareops |> String.join " and ")
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
