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


type Collect a
    = Cont a
    | Stop a


{-| Representations for Elm's expressions.
-}
type Expression
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Variable (List Name)
    | List (List Expression)
    | Tuple (List Expression)
    | Access Expression (List Name)
    | AccessFunction Name
    | Record (List ( Name, Expression ))
    | RecordUpdate Name (List ( Name, Expression ))
    | If Expression Expression Expression
    | Let (List ( Expression, Expression )) Expression
    | Case Expression (List ( Expression, Expression ))
    | Lambda (List Expression) Expression
    | Application Expression Expression
    | BinOp Expression Expression Expression


character : Parser s Expression
character =
    Character <$> between_ (char '\'') anyChar


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
        multiString <|> singleString


integer : Parser s Expression
integer =
    Integer <$> Combine.Num.int


float : Parser s Expression
float =
    Float <$> Combine.Num.float


access : Parser s Expression
access =
    Access <$> variable <*> many1 (Combine.string "." *> loName)


accessFunction : Parser s Expression
accessFunction =
    AccessFunction <$> (Combine.string "." *> loName)


variable : Parser s Expression
variable =
    Variable
        <$> choice
                [ singleton <$> emptyTuple
                , singleton <$> loName
                , sepBy1 (Combine.string ".") upName
                , singleton <$> parens operator
                , singleton <$> parens (Combine.regex ",+")
                ]


list : OpTable -> Parser s Expression
list ops =
    lazy <|
        \() ->
            List <$> brackets (commaSeparated_ (expression ops))


tuple : OpTable -> Parser s Expression
tuple ops =
    lazy <|
        \() ->
            Tuple
                <$> (parens (commaSeparated_ (expression ops))
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
            Record <$> braces (commaSeparated ((,) <$> loName <*> (symbol "=" *> expression ops)))


simplifiedRecord : Parser s Expression
simplifiedRecord =
    lazy <|
        \() ->
            Record <$> (braces (commaSeparated ((\a -> ( a, Variable [ a ] )) <$> loName)))


recordUpdate : OpTable -> Parser s Expression
recordUpdate ops =
    lazy <|
        \() ->
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
            Let
                <$> (symbol "let" *> many1 (letBinding ops))
                <*> (symbol "in" *> expression ops)


letBinding : OpTable -> Parser s ( Expression, Expression )
letBinding ops =
    lazy <|
        \() ->
            (,)
                <$> (between_ whitespace (expression ops))
                <*> (symbol "=" *> expression ops)


ifExpression : OpTable -> Parser s Expression
ifExpression ops =
    lazy <|
        \() ->
            If
                <$> (symbol "if" *> expression ops)
                <*> (symbol "then" *> expression ops)
                <*> (symbol "else" *> expression ops)


caseExpression : OpTable -> Parser s Expression
caseExpression ops =
    lazy <|
        \() ->
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
            Lambda
                <$> (symbol "\\" *> many (between_ spaces (term ops)))
                <*> (symbol "->" *> expression ops)


application : OpTable -> Parser s Expression
application ops =
    lazy <|
        \() ->
            term ops |> chainl (Application <$ spacesOrIndentedNewline ops)


spacesOrIndentedNewline : OpTable -> Parser s String
spacesOrIndentedNewline ops =
    let
        startsBinding =
            or
                (letBinding ops)
                (caseBinding ops)

        failAtBinding =
            maybe startsBinding
                |> andThen
                    (\x ->
                        case x of
                            Just _ ->
                                fail "next line starts a new case or let binding"

                            Nothing ->
                                succeed ""
                    )
    in
        or
            (spaces *> newline *> spaces_ *> lookAhead failAtBinding)
            (spaces_)


binary : OpTable -> Parser s Expression
binary ops =
    lazy <|
        \() ->
            let
                next =
                    between_ whitespace (choice [ operator, symbol "as" ])
                        |> andThen
                            (\op ->
                                choice [ Cont <$> application ops, Stop <$> expression ops ]
                                    |> andThen
                                        (\e ->
                                            case e of
                                                Cont t ->
                                                    ((::) ( op, t )) <$> collect

                                                Stop e ->
                                                    succeed [ ( op, e ) ]
                                        )
                            )

                collect =
                    next <|> succeed []
            in
                application ops
                    |> andThen
                        (\e ->
                            collect
                                |> andThen
                                    (\eops ->
                                        split ops 0 e eops
                                    )
                        )


{-| A parses for term
-}
term : OpTable -> Parser s Expression
term ops =
    lazy <|
        \() ->
            choice
                [ character
                , string
                , float
                , integer
                , access
                , accessFunction
                , variable
                , list ops
                , tuple ops
                , recordUpdate ops
                , record ops
                , simplifiedRecord
                , parens (between_ whitespace (expression ops))
                ]


{-| A parser for Elm expressions.
-}
expression : OpTable -> Parser s Expression
expression ops =
    lazy <|
        \() ->
            choice
                [ letExpression ops
                , caseExpression ops
                , ifExpression ops
                , lambda ops
                , binary ops
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


hasLevel : OpTable -> Int -> ( String, Expression ) -> Bool
hasLevel ops l ( n, _ ) =
    level ops n == l


split : OpTable -> Int -> Expression -> List ( String, Expression ) -> Parser s Expression
split ops l e eops =
    case eops of
        [] ->
            succeed e

        _ ->
            findAssoc ops l eops
                |> andThen
                    (\assoc ->
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
                                        case assoc of
                                            R ->
                                                joinR es ops_

                                            _ ->
                                                joinL es ops_
                                )
                    )


splitLevel : OpTable -> Int -> Expression -> List ( String, Expression ) -> List (Parser s Expression)
splitLevel ops l e eops =
    case break (hasLevel ops l) eops of
        ( lops, ( _, e_ ) :: rops ) ->
            split ops (l + 1) e lops :: splitLevel ops l e_ rops

        ( lops, [] ) ->
            [ split ops (l + 1) e lops ]


joinL : List Expression -> List String -> Parser s Expression
joinL es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, op :: remO ) ->
            joinL ((BinOp (Variable [ op ]) a b) :: remE) remO

        _ ->
            fail ""


joinR : List Expression -> List String -> Parser s Expression
joinR es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, op :: remO ) ->
            joinR (b :: remE) remO
                |> andThen
                    (\e ->
                        succeed (BinOp (Variable [ op ]) a e)
                    )

        _ ->
            fail ""


findAssoc : OpTable -> Int -> List ( String, Expression ) -> Parser s Assoc
findAssoc ops l eops =
    let
        lops =
            List.filter (hasLevel ops l) eops

        assocs =
            List.map (assoc ops << Tuple.first) lops

        error issue =
            let
                operators =
                    List.map Tuple.first lops |> String.join " and "
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
