module Ast exposing (parseExpression, parseStatement, parseOpTable, parseModule, parse, parsePattern)

{-| This module exposes functions for parsing Elm code.


# Parsers

@docs parseExpression, parseStatement, parseOpTable, parseModule, parse, parsePattern

-}

import Ast.BinOp exposing (OpTable, operators)
import Ast.Common exposing (MPattern, Pattern(..))
import Ast.Expression exposing (MExp, expression)
import Ast.Pattern exposing (pattern)
import Ast.Statement exposing (Statement, opTable, statement, statements)
import Combine exposing (ParseError, ParseOk, end, ignore)


{-| Parse an Elm pattern
-}
parsePattern : String -> Result (ParseError ()) (ParseOk () MPattern)
parsePattern =
    Combine.parse (pattern |> ignore end)


{-| Parse an Elm expression.
-}
parseExpression : OpTable -> String -> Result (ParseError ()) (ParseOk () MExp)
parseExpression ops =
    Combine.parse (expression ops |> ignore end)


{-| Parse an Elm statement.
-}
parseStatement : OpTable -> String -> Result (ParseError ()) (ParseOk () Statement)
parseStatement ops =
    Combine.parse (statement ops |> ignore end)


{-| Parse an OpTable from a module.
-}
parseOpTable : OpTable -> String -> Result (ParseError ()) (ParseOk () OpTable)
parseOpTable ops =
    Combine.parse (opTable ops)


{-| Parse an Elm module.
-}
parseModule : OpTable -> String -> Result (ParseError ()) (ParseOk () (List Statement))
parseModule ops =
    Combine.parse (statements ops)


{-| Parse an Elm module, scanning for infix declarations first.
-}
parse : String -> Result (ParseError ()) (ParseOk () (List Statement))
parse input =
    case parseOpTable operators input of
        Ok ( state, stream, ops ) ->
            parseModule ops input

        Err e ->
            Err e
