module Ast.Expression exposing
    ( Expression(..), MExp, ExpressionSansMeta(..)
    , expression, dropMExpMeta, dropExpressionMeta
    , term
    )

{-| This module exposes parsers for Elm expressions.


# Types

@docs Expression, MExp, ExpressionSansMeta


# rs

@docs expression, dropMExpMeta, dropExpressionMeta


# Expression

@docs term

-}

import Ast.BinOp exposing (..)
import Ast.Helpers exposing (..)
import Char
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Dict exposing (Dict)
import Hex
import List exposing (singleton)
import List.Extra exposing (break)
import String


type Collect a
    = Cont a
    | Stop a


{-| Representation for Elm's expression containing it's meta information
-}
type alias MExp =
    WithMeta Expression


type alias Operator =
    WithMeta String


{-| Representations for Elm's expressions.
-}
type Expression
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Variable (List Name)
    | List (List MExp)
    | Tuple (List MExp)
    | Access MExp (List (WithMeta Name))
    | AccessFunction Name
    | Record (List ( WithMeta Name, MExp ))
    | RecordUpdate (WithMeta Name) (List ( WithMeta Name, MExp ))
    | If MExp MExp MExp
    | Let (List ( MExp, MExp )) MExp
    | Case MExp (List ( MExp, MExp ))
    | Lambda (List MExp) MExp
    | Application MExp MExp
    | BinOp MExp MExp MExp


{-| Test only
-}
type ExpressionSansMeta
    = CharacterSM Char
    | StringSM String
    | IntegerSM Int
    | FloatSM Float
    | VariableSM (List Name)
    | ListSM (List ExpressionSansMeta)
    | TupleSM (List ExpressionSansMeta)
    | AccessSM ExpressionSansMeta (List Name)
    | AccessFunctionSM Name
    | RecordSM (List ( Name, ExpressionSansMeta ))
    | RecordUpdateSM Name (List ( Name, ExpressionSansMeta ))
    | IfSM ExpressionSansMeta ExpressionSansMeta ExpressionSansMeta
    | LetSM (List ( ExpressionSansMeta, ExpressionSansMeta )) ExpressionSansMeta
    | CaseSM ExpressionSansMeta (List ( ExpressionSansMeta, ExpressionSansMeta ))
    | LambdaSM (List ExpressionSansMeta) ExpressionSansMeta
    | ApplicationSM ExpressionSansMeta ExpressionSansMeta
    | BinOpSM ExpressionSansMeta ExpressionSansMeta ExpressionSansMeta


{-| Test only
-}
dropMExpMeta : MExp -> ExpressionSansMeta
dropMExpMeta { meta, e } =
    dropExpressionMeta e


dropWithMetaMExp : ( WithMeta a, MExp ) -> ( a, ExpressionSansMeta )
dropWithMetaMExp ( a, b ) =
    ( dropMeta a, dropMExpMeta b )


dropDoubleMExp : ( MExp, MExp ) -> ( ExpressionSansMeta, ExpressionSansMeta )
dropDoubleMExp ( a, b ) =
    ( dropMExpMeta a, dropMExpMeta b )


{-| Test only
-}
dropExpressionMeta : Expression -> ExpressionSansMeta
dropExpressionMeta e =
    case e of
        Character c ->
            CharacterSM c

        String s ->
            StringSM s

        Integer i ->
            IntegerSM i

        Float f ->
            FloatSM f

        Variable l ->
            VariableSM l

        List l ->
            ListSM (List.map dropMExpMeta l)

        Tuple l ->
            TupleSM (List.map dropMExpMeta l)

        Access ex l ->
            AccessSM (dropMExpMeta ex) (List.map dropMeta l)

        AccessFunction n ->
            AccessFunctionSM n

        Record l ->
            RecordSM (List.map dropWithMetaMExp l)

        RecordUpdate n l ->
            RecordUpdateSM (dropMeta n) (List.map dropWithMetaMExp l)

        If e1 e2 e3 ->
            IfSM (dropMExpMeta e1) (dropMExpMeta e2) (dropMExpMeta e3)

        Let l e ->
            LetSM (List.map dropDoubleMExp l) (dropMExpMeta e)

        Case e l ->
            CaseSM (dropMExpMeta e) (List.map dropDoubleMExp l)

        Lambda l e ->
            LambdaSM (List.map dropMExpMeta l) (dropMExpMeta e)

        Application e1 e2 ->
            ApplicationSM (dropMExpMeta e1) (dropMExpMeta e2)

        BinOp e1 e2 e3 ->
            BinOpSM (dropMExpMeta e1) (dropMExpMeta e2) (dropMExpMeta e3)


generateIds_ : Int -> MExp -> ( Int, MExp )
generateIds_ newId ( _, l, c, e ) =
    let
        trivialId i l c e = (i + 1, (Just i, l, c, e))

        genNameListIds newId =
            List.foldr (\( _, l, c, n ) ( accId, acc ) -> ( accId + 1, ( accId, l, c, n ) :: acc )) ( newId + 1, [] )

        genListIds newId =
            List.foldr (\x ( accId, acc ) -> generateIds_ accId x |> (\( maxId, exp ) -> ( maxId, exp :: acc ))) ( newId + 1, [] )

        genRecordsIds newId records =
            let
                ( names, exps ) =
                    List.unzip records

                ( namesId, newNames ) =
                    genNameListIds newId names

                ( expsId, newExps ) =
                    genListIds namesId exps
            in
            ( expsId, List.map2 (\x y -> ( x, y )) newNames newExps )

        genLetCase newId li exp =
            let
                ( newerId, newExp ) =
                    generateIds_ newId exp

                ( li1, li2 ) =
                    List.unzip li

                ( li1Id, newLi1 ) =
                    genListIds newerId li1

                ( li2Id, newLi2 ) =
                    genListIds li1Id li2
            in
            ( li2Id, newExp, List.map2 (\x y -> ( x, y )) newLi1 newLi2 )
    in
    case e of
        Character _ ->
            trivialId newId l c e

        String _ ->
            trivialId newId l c e

        Integer _ ->
            trivialId newId l c e

        Float _ ->
            trivialId newId l c e

        Variable _ ->
            trivialId newId l c e

        List li ->
            let
                ( maxId, exp ) =
                    genListIds ( newId + 1, li )
            in
            ( maxId, ( Just newId, l, c, List exp ) )

        Tuple li ->
            let
                ( maxId, exp ) =
                    genListIds ( newId + 1, li )
            in
            ( maxId, ( Just newId, l, c, Tuple exp ) )

        Access exp li ->
            let
                ( maxId, newExp :: newLi ) =
                    genListIds (newId + 1) (exp :: li)
            in
            ( maxId, ( Just newId, l, c, Access newExp newLi ) )

        AccessFunction _ ->
            trivialId newId l c e

        Record records ->
            let
                ( maxId, newRecords ) =
                    genRecordsIds (newId + 1) records
            in
            ( maxId, ( Just newId, l, c, Record newRecords ) )

        RecordUpdate name records ->
            let
                ( recordsId, newRecords ) =
                    genRecordsIds (newId + 1) records

                ( _, nameL, nameC, nameN ) =
                    name

                newName =
                    ( recordsId, nameL, nameC, nameN )

                maxId =
                    recordsId + 1
            in
            ( maxId, ( Just newId, l, c, RecordUpdate newName newRecords ) )

        If e1 e2 e3 ->
            let
                ( maxId, newE1 :: newE2 :: newE3 :: _ ) =
                    genListIds (newId + 1) [ e1, e2, e3 ]
            in
            ( maxId, ( Just newId, l, c, If newE1 newE2 newE3 ) )

        Let li exp ->
            let
                ( maxId, newExp, newLi ) =
                    genLetCase (newId + 1) li exp
            in
            ( maxId, ( Just newId, l, c, Let newLi newExp ) )

        Case exp li ->
            let
                ( maxId, newExp, newLi ) =
                    genLetCase (newId + 1) li exp
            in
            ( maxId, ( Just newId, l, c, Case newExp newLi ) )

        Lambda li exp ->
            let
                ( maxId, newExp :: newLi ) =
                    genListIds (newId + 1) (exp :: li)
            in
            ( maxId, ( Just newId, l, c, Lambda newLi newExp ) )

        Application e1 e2 ->
            let
                ( maxId, newE1 :: newE2 :: _ ) =
                    genListIds (newId + 1) [ e1, e2 ]
            in
            ( maxId, ( Just newId, l, c, Application newE1 newE2 ) )

        BinOp e1 e2 e3 ->
            let
                ( maxId, newE1 :: newE2 :: newE3 :: _ ) =
                    genListIds (newId + 1) [ e1, e2, e3 ]
            in
            ( maxId, ( Just newId, l, c, BinOp newE1 newE2 newE3 ) )


generateIds : MExp -> MExp
generateIds =
    Tuple.second generateIds_ 0


character : Parser s MExp
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


string : Parser s MExp
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


integer : Parser s MExp
integer =
    withMeta <| Integer <$> Combine.Num.int


float : Parser s MExp
float =
    withMeta <| Float <$> Combine.Num.float


access : Parser s MExp
access =
    withMeta <| Access <$> variable <*> many1 (Combine.string "." *> withMeta loName)


accessFunction : Parser s MExp
accessFunction =
    withMeta <| AccessFunction <$> (Combine.string "." *> loName)


variable : Parser s MExp
variable =
    withMeta <|
        Variable
            <$> choice
                    [ singleton <$> loName
                    , sepBy1 (Combine.string ".") upName
                    , singleton <$> parens operator
                    , singleton <$> parens (Combine.regex ",+")
                    , singleton <$> emptyTuple
                    ]


list : OpTable -> Parser s MExp
list ops =
    lazy <|
        \() ->
            withMeta <| List <$> brackets (commaSeparated_ <| expression ops)


tuple : OpTable -> Parser s MExp
tuple ops =
    lazy <|
        \() ->
            withMeta <|
                Tuple
                    <$> (parens (commaSeparated_ <| expression ops)
                            >>= (\a ->
                                    case a of
                                        [ _ ] ->
                                            fail "No single tuples"

                                        anyOther ->
                                            succeed anyOther
                                )
                        )


record : OpTable -> Parser s MExp
record ops =
    lazy <|
        \() ->
            withMeta <| Record <$> braces (commaSeparated ((,) <$> withMeta loName <*> (symbol "=" *> expression ops)))


simplifiedRecord : Parser s MExp
simplifiedRecord =
    let
        branch { line, column } =
            loName
                |> map (\a -> ( WithMeta (Nothing, line, column, a), Ast.Helpers.WithMeta (Nothing, line, column, Variable [ a ]) ))

                --  withLocation (\a -> (\x -> (Nothing, a.line, a.column, x)) <$> p)

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


letBinding : OpTable -> Parser s ( MExp, MExp )
letBinding ops =
    lazy <|
        \() ->
            (,)
                <$> (between_ whitespace <| expression ops)
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
                        <$> (exactIndentation indent *> expression ops)
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


countIndent : Parser s Int
countIndent =
    newline *> spaces >>= (String.filter (\char -> char == ' ') >> String.length >> succeed)


lambda : OpTable -> Parser s MExp
lambda ops =
    lazy <|
        \() ->
            withMeta <|
                Lambda
                    <$> (symbol "\\" *> many (between_ spaces <| term ops))
                    <*> (symbol "->" *> expression ops)


application : OpTable -> Parser s MExp
application ops =
    lazy <|
        \() ->
            withLocation (\l -> chainl ((\a b -> WithMeta (Nothing, l.line, l.column, Application a b)) <$ spacesOrIndentedNewline (l.column + 1)) (term ops))


spacesOrIndentedNewline : Int -> Parser s ()
spacesOrIndentedNewline indentation =
    lazy <|
        \() ->
            or (spaces_ *> succeed ())
                (countIndent
                    >>= (\column ->
                            if column < indentation then
                                fail "Arguments have to be at least the same indentation as the function"

                            else
                                succeed ()
                        )
                )


operatorOrAsBetween : Parser s (WithMeta String)
operatorOrAsBetween =
    lazy <|
        \() ->
            withMeta <| between_ whitespace <| operator <|> symbol_ "as"


successOrEmptyList : Parser s (List a) -> Parser s (List a)
successOrEmptyList p =
    lazy <| \() -> choice [ p, succeed [] ]


binary : OpTable -> Parser s MExp
binary ops =
    lazy <|
        \() ->
            let
                next =
                    operatorOrAsBetween
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
    level ops n.e == l


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

        ( a :: b :: remE, op :: remO ) ->
            joinL (WithMeta op.meta (BinOp (WithMeta op.meta <| Variable [ op.e ]) a b) :: remE) remO

        _ ->
            fail ""


joinR : List MExp -> List Operator -> Parser s MExp
joinR es ops =
    case ( es, ops ) of
        ( [ e ], [] ) ->
            succeed e

        ( a :: b :: remE, op :: remO ) ->
            joinR (b :: remE) remO
                |> andThen
                    (\e ->
                        succeed (WithMeta op.meta <| BinOp (WithMeta op.meta <| Variable [ op.e ]) a e)
                    )

        _ ->
            fail ""


findAssoc : OpTable -> Int -> List ( Operator, MExp ) -> Parser s Assoc
findAssoc ops l eops =
    let
        lops =
            List.filter (hasLevel ops l) eops

        assocs =
            List.map (assoc ops << .e << Tuple.first) lops

        error issue =
            let
                operators =
                    List.map (Tuple.first >> .e) lops |> String.join " and "
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
