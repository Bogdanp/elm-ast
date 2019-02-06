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
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Statement(..), Type(..))
import Expect exposing (..)


var a =
    Variable [ a ]


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
            Expect.equal e r

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isApplication : Expression -> List Expression -> String -> Expectation
isApplication fn args =
    isExpression (List.foldl (flip Application) fn args)


isStatement : Statement -> String -> Expectation
isStatement s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


areStatements : List Statement -> String -> Expectation
areStatements s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)
