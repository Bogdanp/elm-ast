module Helpers exposing
    ( areStatements
    , fails
    , isApplication
    , isExpression
    , isStatement
    , var
    )

import Ast exposing (parse, parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (Expression(..), MExp)
import Ast.Statement exposing (ExportSet(..), Statement(..), Type(..))
import Expect exposing (..)



-- Structures


var : Int -> Int -> String -> MExp
var line column a =
    { e = Variable [ a ], meta = { line = line, column = column, source = a } }



-- Helpers


fails : String -> Expectation
fails s =
    case parseExpression operators s of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail (s ++ " expected to fail")


isExpression : Expression -> String -> Expectation
isExpression e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e r.e

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isApplication : Expression -> List Expression -> String -> Expectation
isApplication fn args i =
    let
        appToList app =
            case app.e of
                Application a b ->
                    [ a.e ] ++ appToList b

                e ->
                    [ e ]
    in
    case parseExpression operators (String.trim i) of
        Ok ( _, _, app ) ->
            Expect.equal (fn :: args) (appToList app)

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isStatement : Statement -> String -> Expectation
isStatement s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        _ ->
            Expect.fail ("failed to parse: \"" ++ i ++ "\" <vs> " ++ toString s)


areStatements : List Statement -> String -> Expectation
areStatements s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        _ ->
            Expect.fail ("failed to parse: \"" ++ i ++ "\" <vs> " ++ toString s)
