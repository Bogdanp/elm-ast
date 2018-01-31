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
import List exposing (singleton)
import List.Extra exposing (break)
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
                [ singleton <$> loName
                , sepBy1 (Combine.string ".") upName
                , singleton <$> parens operator
                , singleton <$> parens (Combine.regex ",+")
                , singleton <$> emptyTuple
                ]


list : OpTable -> Parser s Expression
list ops =
    lazy <|
        \() ->
            List <$> brackets (commaSeparated_ <| expression ops)


tuple : OpTable -> Parser s Expression
tuple ops =
    lazy <|
        \() ->
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
            If
                <$> (symbol "if" *> expression ops)
                <*> (symbol "then" *> expression ops)
                <*> (symbol "else" *> expression ops)


caseExpression : OpTable -> Parser s Expression
caseExpression ops =
    let
        binding indent =
            lazy <|
                \() ->
                    (,)
                        <$> (exactIndentation indent *> expression ops)
                        <*> (symbol "->" *> expression ops)
    in
        lazy <|
            \() ->
                Case
                    <$> (symbol "case" *> expression ops)
                    <*> (whitespace
                            *> Combine.string "of"
                            *> lookAhead countIndent
                            >>= \indent ->
                                    many1 (binding indent)
                        )


countIndent : Parser s Int
countIndent =
    whitespace >>= (String.filter (\char -> char == ' ') >> String.length >> succeed)


lambda : OpTable -> Parser s Expression
lambda ops =
    lazy <|
        \() ->
            Lambda
                <$> (symbol "\\" *> many (between_ spaces <| term ops))
                <*> (symbol "->" *> expression ops)


application : OpTable -> Parser s Expression
application ops =
    lazy <|
        \() ->
            withColumn (\l -> chainl (Application <$ spacesOrIndentedNewline l) (term ops))


negate : Maybe a -> Parser s String
negate x =
    case x of
        Just _ ->
            -- next line starts a new case or let binding
            fail ""

        Nothing ->
            succeed ""


spacesOrIndentedNewline : Int -> Parser s String
spacesOrIndentedNewline indentation =
    lazy <|
        \() ->
            or spaces_ <|
                countIndent
                    >>= \column ->
                            if column < indentation then
                                fail "Arguments have to be at least the same indentation as the function"
                            else
                                succeed ""


operatorOrAsBetween : Parser s String
operatorOrAsBetween =
    lazy <|
        \() ->
            between_ whitespace <| operator <|> symbol_ "as"


successOrEmptyList : Parser s (List a) -> Parser s (List a)
successOrEmptyList p =
    lazy <| \() -> choice [ p, succeed [] ]


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
                                                            ((::) ( op, t )) <$> successOrEmptyList next

                                                        Stop ex ->
                                                            succeed [ ( op, ex ) ]
            in
                application ops
                    >>= (\e -> successOrEmptyList next >>= \eops -> split ops 0 e eops)


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
