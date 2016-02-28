module Ast ( parseExpression, parseStatement, parse ) where

{-| This module exposes functions for parsing Elm code.

# Parsers
@docs parseExpression, parseStatement, parse

-}

import Combine exposing (Context, end)
import Combine.Infix exposing ((<*))

import Ast.Expression exposing (Expression, expression)
import Ast.Helpers exposing (OpTable)
import Ast.Module exposing (Module, module')
import Ast.Statement exposing (Statement, statement)


{-| Parse an Elm expression. -}
parseExpression : OpTable -> String -> (Result (List String) Expression, Context)
parseExpression ops = Combine.parse (expression ops <* end)


{-| Parse an Elm statement. -}
parseStatement : OpTable -> String -> (Result (List String) Statement, Context)
parseStatement ops = Combine.parse (statement ops <* end)


{-| Parse an Elm module. -}
parse : String -> (Result (List String) Module, Context)
parse = Combine.parse (module' <* end)
