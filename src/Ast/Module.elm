module Ast.Module ( Module, module', opTable ) where

{-| This module exposes parsers for Elm modules.

# Types
@docs Module

# Parsers
@docs module'

# Misc
@docs opTable

-}

import Combine exposing (..)
import Combine.Infix exposing (..)
import Dict

import Ast.Exports exposing (Exports(..), exports)
import Ast.Helpers exposing (..)
import Ast.Statement exposing (Statement(..), statement)

{-| FIXME -}
type alias Module
  = { name : ModuleName
    , exports : Exports
    , opTable : OpTable
    , statements : List Statement
    }

statements : OpTable -> Parser (List Statement)
statements ops =
  many1 (statement ops <* whitespace)

moduleDeclaration : Parser Statement
moduleDeclaration =
  ModuleDeclaration
    <$> (initialSymbol "module" *> moduleName)
    <*> (optional AllExport exports <* symbol "where" <* whitespace)


{-| A parser for Elm modules. -}
module' : Parser Module
module' =
  let
    moduleDeclaration' =
      optional (ModuleDeclaration ["Main"] AllExport) moduleDeclaration

    cons s xs =
      case s of
        ModuleDeclaration n e -> Module n e opTable xs
        _ -> Debug.crash "impossible"
  in
    cons <$> moduleDeclaration' <*> statements opTable


{-| The default operator precedence table. -}
opTable : OpTable
opTable =
  Dict.empty
    |> Dict.insert "||"  (L, 2)
    |> Dict.insert "&&"  (L, 3)
    |> Dict.insert "=="  (L, 4)
    |> Dict.insert "/="  (L, 4)
    |> Dict.insert "<"   (L, 4)
    |> Dict.insert ">"   (L, 4)
    |> Dict.insert ">="  (L, 4)
    |> Dict.insert "<="  (L, 4)
    |> Dict.insert "++"  (L, 5)
    |> Dict.insert "+"   (L, 6)
    |> Dict.insert "-"   (L, 6)
    |> Dict.insert "*"   (L, 7)
    |> Dict.insert "/"   (L, 7)
    |> Dict.insert "%"   (L, 7)
    |> Dict.insert "//"  (L, 7)
    |> Dict.insert "rem" (L, 7)
    |> Dict.insert "^"   (L, 8)
    |> Dict.insert "<<"  (L, 9)
    |> Dict.insert ">>"  (L, 9)
    |> Dict.insert "<|"  (R, 0)
    |> Dict.insert "|>"  (R, 0)
