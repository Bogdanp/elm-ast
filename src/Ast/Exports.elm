module Ast.Exports ( Exports(..), exports ) where

import Combine exposing (..)
import Combine.Infix exposing (..)

import Ast.Helpers exposing (..)

type Exports
  = AllExport
  | SubsetExport (List Exports)
  | FunctionExport Name
  | TypeExport Name (Maybe Exports)

allExport : Parser Exports
allExport =
  AllExport <$ symbol ".."

functionExport : Parser Exports
functionExport =
  FunctionExport <$> functionName

constructorSubsetExports : Parser Exports
constructorSubsetExports =
  SubsetExport <$> commaSeparated (FunctionExport <$> upName)

constructorExports : Parser (Maybe Exports)
constructorExports =
  maybe <| parens <| choice [ allExport
                            , constructorSubsetExports
                            ]

typeExport : Parser Exports
typeExport =
  TypeExport <$> (upName <* spaces) <*> constructorExports

subsetExport : Parser Exports
subsetExport =
  SubsetExport
    <$> commaSeparated (functionExport `or` typeExport)

exports : Parser Exports
exports =
  parens <| choice [ allExport, subsetExport ]
