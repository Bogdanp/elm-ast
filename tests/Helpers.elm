module Helpers exposing
    ( app
    , areStatements
    , areStatementsSansMeta
    , binOp
    , case_
    , fails
    , fakeMeta
    , hasUniqueIds
    , integer
    , isApplicationSansMeta
    , isExpression
    , isExpressionSansMeta
    , isStatement
    , isStatementSansMeta
    , list
    , record
    , recordUpdate
    , simpleParse
    , tuple
    , var
    )

import Ast exposing (parse, parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (Expression(..), ExpressionSansMeta(..), Id, MExp, WithMeta, dropExpressionMeta, dropId, dropMExpMeta, getId)
import Ast.Helpers exposing (Name)
import Ast.Statement exposing (ExportSet(..), Statement(..), Type(..), dropStatementMeta)
import Expect exposing (..)
import Set exposing (Set)



-- Structures


app : MExp -> MExp -> MExp
app left right =
    fakeMeta (Application left right)


record =
    fakeMeta << Record


recordUpdate : String -> List ( WithMeta String, MExp ) -> MExp
recordUpdate name =
    fakeMeta << RecordUpdate (fakeMeta name)


var : String -> MExp
var name =
    mvar 0 0 name


integer =
    fakeMeta << Integer


binOp name l r =
    fakeMeta <| BinOp name l r


tuple =
    fakeMeta << Tuple


case_ mexp cases =
    fakeMeta <| Case mexp cases


list =
    fakeMeta << List


mvar : Int -> Int -> String -> MExp
mvar line column name =
    ( Nothing, line, column, Variable [ name ] )



-- Helpers


fails : String -> Expectation
fails s =
    case parseExpression operators s of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail (s ++ " expected to fail")


checkListUniqueIds : List MExp -> Set Id -> Maybe (Set Id)
checkListUniqueIds li ids =
    List.foldl (\x acc -> Maybe.andThen (hasUniqueIds_ x) acc) (Just ids) li


checkNameId : WithMeta Name -> Set Id -> Maybe (Set Id)
checkNameId ( id, _, _, _ ) ids =
    case id of
        Nothing ->
            Nothing

        Just actualId ->
            if Set.member actualId ids then
                Nothing

            else
                Just <| Set.insert actualId ids


checkNameListUniqueIds : List (WithMeta Name) -> Set Id -> Maybe (Set Id)
checkNameListUniqueIds li ids =
    case li of
        [] ->
            Just ids

        n :: xs ->
            checkNameId n ids |> Maybe.andThen (checkNameListUniqueIds xs)


checkListAndExp : List MExp -> MExp -> Set Id -> Maybe (Set Id)
checkListAndExp li ex ids =
    hasUniqueIds_ ex ids |> Maybe.andThen (checkListUniqueIds li)


checkRecordsIds : List ( WithMeta Name, MExp ) -> Set Id -> Maybe (Set Id)
checkRecordsIds records ids =
    List.unzip records
        |> (\( names, exps ) ->
                checkNameListUniqueIds names ids
                    |> Maybe.andThen
                        (\newIds -> checkListUniqueIds exps newIds)
           )


checkLetCase : List ( MExp, MExp ) -> MExp -> Set Id -> Maybe (Set Id)
checkLetCase li exp ids =
    let
        ( li1, li2 ) =
            List.unzip li
    in
    checkListUniqueIds li1 ids |> Maybe.andThen (checkListAndExp li2 exp)


hasUniqueIds_ : MExp -> Set Id -> Maybe (Set Id)
hasUniqueIds_ ( id, _, _, e ) ids =
    case id of
        Nothing ->
            Nothing

        Just actualId ->
            if Set.member actualId ids then
                Just (Set.insert actualId ids)

            else
                case e of
                    Character _ ->
                        Just (Set.insert actualId ids)

                    String _ ->
                        Just (Set.insert actualId ids)

                    Integer _ ->
                        Just (Set.insert actualId ids)

                    Float _ ->
                        Just (Set.insert actualId ids)

                    Variable _ ->
                        Just (Set.insert actualId ids)

                    List li ->
                        checkListUniqueIds li ids

                    Tuple li ->
                        checkListUniqueIds li ids

                    Access exp li ->
                        hasUniqueIds_ exp ids |> Maybe.andThen (checkNameListUniqueIds li)

                    AccessFunction _ ->
                        Just (Set.insert actualId ids)

                    Record records ->
                        checkRecordsIds records ids

                    RecordUpdate n records ->
                        checkNameId n ids
                            |> Maybe.andThen (checkRecordsIds records)

                    If e1 e2 e3 ->
                        hasUniqueIds_ e1 ids
                            |> Maybe.andThen (hasUniqueIds_ e2)
                            |> Maybe.andThen (hasUniqueIds_ e3)

                    Let li exp ->
                        checkLetCase li exp ids

                    Case exp li ->
                        checkLetCase li exp ids

                    Lambda li exp ->
                        hasUniqueIds_ exp ids |> Maybe.andThen (checkListUniqueIds li)

                    Application e1 e2 ->
                        hasUniqueIds_ e1 ids
                            |> Maybe.andThen (hasUniqueIds_ e2)

                    BinOp e1 e2 e3 ->
                        hasUniqueIds_ e1 ids
                            |> Maybe.andThen (hasUniqueIds_ e2)
                            |> Maybe.andThen (hasUniqueIds_ e3)


simpleParse : String -> Result String MExp
simpleParse i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, e ) ->
            Ok e

        Err ( _, { position }, es ) ->
            Err ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


hasUniqueIds : MExp -> Expectation
hasUniqueIds exp =
    case hasUniqueIds_ exp Set.empty of
        Nothing ->
            Expect.fail "Ids are not unique"

        _ ->
            Expect.pass


isExpression : MExp -> String -> Expectation
isExpression e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal (dropId e) (dropId r)

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isExpressionSansMeta : MExp -> String -> Expectation
isExpressionSansMeta e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal (dropMExpMeta e) (dropMExpMeta r)

        Err ( _, a, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString a.position ++ " rest: |" ++ a.input ++ "| with errors: " ++ toString es)


isApplicationSansMeta : MExp -> List MExp -> String -> Expectation
isApplicationSansMeta fn args i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, app ) ->
            let
                l =
                    List.foldl (flip ApplicationSM)
                        (dropMExpMeta fn)
                        (List.map dropMExpMeta args)

                r =
                    dropMExpMeta app
            in
            Expect.equal l r

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isStatement : Statement -> String -> Expectation
isStatement s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isStatementSansMeta : Statement -> String -> Expectation
isStatementSansMeta s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal (dropStatementMeta r) (dropStatementMeta s)

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


areStatements : List Statement -> String -> Expectation
areStatements s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


areStatementsSansMeta : List Statement -> String -> Expectation
areStatementsSansMeta s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal (List.map dropStatementMeta r) (List.map dropStatementMeta s)

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


fakeMeta : a -> WithMeta a
fakeMeta e =
    ( Nothing, 1, 1, e )
