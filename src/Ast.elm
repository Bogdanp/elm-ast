module Ast ( parseExpression, parseStatement, parseOpTable, parseModule, parse ) where

{-| This module exposes functions for parsing Elm code.

# Parsers
@docs parseExpression, parseStatement, parseOpTable, parseModule, parse

-}

import Combine exposing (Context, end)
import Combine.Infix exposing ((<*))

import Ast.BinOp exposing (OpTable, operators)
import Ast.Expression exposing (Expression, expression)
import Ast.Statement exposing (Statement, statement, statements, opTable)


{-| Parse an Elm expression. -}
parseExpression : OpTable -> String -> (Result (List String) Expression, Context)
parseExpression ops = Combine.parse (expression ops <* end)


{-| Parse an Elm statement. -}
parseStatement : OpTable -> String -> (Result (List String) Statement, Context)
parseStatement ops = Combine.parse (statement ops <* end)


{-| Parse an OpTable from a module. -}
parseOpTable : OpTable -> String -> (Result (List String) OpTable, Context)
parseOpTable ops = Combine.parse (opTable ops)


{-| Parse an Elm module. -}
parseModule : OpTable -> String -> (Result (List String) (List Statement), Context)
parseModule ops = Combine.parse (statements ops)


{-| Parse an Elm module, scanning for infix declarations first. -}
parse : String -> (Result (List String) (List Statement), Context)
parse input =
  case parseOpTable operators input of
    (Ok ops, _) ->
      parseModule ops input

    (Err ms, cx) ->
      (Err ms, cx)
