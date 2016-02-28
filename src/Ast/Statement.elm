module Ast.Statement ( ExportSet(..), Type(..), Statement(..)
                     , statement, exports
                     ) where

-- TODO: Fixity declarations

{-| This module exposes parsers for Elm statements.

# Types
@docs ExportSet, Type, Statement

# Parsers
@docs statement, exports

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num
import String

import Ast.Expression exposing (Expression, expression)
import Ast.Helpers exposing (..)

{-| Representations for modules' exports. -}
type ExportSet
  = AllExport
  | SubsetExport (List ExportSet)
  | FunctionExport Name
  | TypeExport Name (Maybe ExportSet)

{-| Representations for Elm's type syntax. -}
type Type
  = TypeConstructor Name (List Type)
  | TypeVariable Name
  | TypeRecordConstructor Type (List (Name, Type))
  | TypeRecord (List (Name, Type))
  | TypeTuple (List Type)
  | TypeApplication Type Type

{-| Representations for Elm's statements. -}
type Statement
  = ModuleDeclaration ModuleName ExportSet
  | ImportStatement ModuleName (Maybe Alias) (Maybe ExportSet)
  | TypeAliasDeclaration Type Type
  | TypeDeclaration Type (List Type)
  | PortTypeDeclaration Name Type
  | PortDeclaration Name (List Name) Expression
  | FunctionTypeDeclaration Name Type
  | FunctionDeclaration Name (List Name) Expression
  | Comment String


allExport : Parser ExportSet
allExport =
  AllExport <$ symbol ".."

functionExport : Parser ExportSet
functionExport =
  FunctionExport <$> functionName

constructorSubsetExports : Parser ExportSet
constructorSubsetExports =
  SubsetExport <$> commaSeparated (FunctionExport <$> upName)

constructorExports : Parser (Maybe ExportSet)
constructorExports =
  maybe <| parens <| choice [ allExport
                            , constructorSubsetExports
                            ]

typeExport : Parser ExportSet
typeExport =
  TypeExport <$> (upName <* spaces) <*> constructorExports

subsetExport : Parser ExportSet
subsetExport =
  SubsetExport
    <$> commaSeparated (functionExport `or` typeExport)

{-| -}
exports : Parser ExportSet
exports =
  parens <| choice [ allExport, subsetExport ]

typeVariable : Parser Type
typeVariable =
  TypeVariable <$> regex "[a-z]+"

typeConstant : Parser Type
typeConstant =
  TypeConstructor <$> upName <*> succeed []

typeApplication : Parser (Type -> Type -> Type)
typeApplication =
  TypeApplication <$ symbol "->"

typeTuple : Parser Type
typeTuple =
  rec <| \() ->
    TypeTuple <$> parens (commaSeparated type')

typeRecordPair : Parser (Name, Type)
typeRecordPair =
  rec <| \() ->
    (,) <$> (loName <* symbol ":") <*> typeAnnotation

typeRecordPairs : Parser (List (Name, Type))
typeRecordPairs =
  rec <| \() ->
    commaSeparated typeRecordPair

typeRecordConstructor : Parser Type
typeRecordConstructor =
  rec <| \() ->
    braces
      <| TypeRecordConstructor
           <$> (between' spaces typeVariable)
           <*> (symbol "|" *> typeRecordPairs)

typeRecord : Parser Type
typeRecord =
  rec <| \() ->
    braces
      <| TypeRecord <$> typeRecordPairs

typeParameter : Parser Type
typeParameter =
  rec <| \() ->
    between' spaces <| choice [ typeVariable
                              , typeConstant
                              , typeRecordConstructor
                              , typeRecord
                              , typeTuple
                              , parens typeAnnotation
                              ]

typeConstructor : Parser Type
typeConstructor =
  rec <| \() ->
    TypeConstructor <$> upName <*> many typeParameter

type' : Parser Type
type' =
  rec <| \() ->
    between' spaces <| choice [ typeConstructor
                              , typeVariable
                              , typeRecordConstructor
                              , typeRecord
                              , typeTuple
                              , parens typeAnnotation
                              ]

typeAnnotation : Parser Type
typeAnnotation =
  rec <| \() ->
    type' `chainl` typeApplication

moduleAlias : Parser Alias
moduleAlias = symbol "as" *> upName

importStatement : Parser Statement
importStatement =
  ImportStatement
    <$> (initialSymbol "import" *> moduleName)
    <*> maybe moduleAlias
    <*> maybe (symbol "exposing" *> exports)


typeAliasDeclaration : Parser Statement
typeAliasDeclaration =
  TypeAliasDeclaration
    <$> (initialSymbol "type" *> symbol "alias" *> type')
    <*> (whitespace *> symbol "=" *> typeAnnotation)

typeDeclaration : Parser Statement
typeDeclaration =
  TypeDeclaration
    <$> (initialSymbol "type" *> type')
    <*> (whitespace *> symbol "=" *> (sepBy1 (symbol "|") (between' whitespace typeConstructor)))

portTypeDeclaration : Parser Statement
portTypeDeclaration =
  PortTypeDeclaration
    <$> (initialSymbol "port" *> loName)
    <*> (symbol ":" *> typeAnnotation)

portDeclaration : OpTable -> Parser Statement
portDeclaration ops =
  PortDeclaration
    <$> (initialSymbol "port" *> loName)
    <*> (many <| between' spaces loName)
    <*> (symbol "=" *> expression ops)

functionTypeDeclaration : Parser Statement
functionTypeDeclaration =
  FunctionTypeDeclaration <$> (loName <* symbol ":") <*> typeAnnotation

functionDeclaration : OpTable -> Parser Statement
functionDeclaration ops =
  FunctionDeclaration
    <$> loName
    <*> (many (between' whitespace loName))
    <*> (symbol "=" *> whitespace *> expression ops)

singleLineComment : Parser Statement
singleLineComment =
  Comment <$> (string "--" *> regex ".*$")

multiLineComment : Parser Statement
multiLineComment =
  (Comment << String.fromList) <$> (string "{-" *> manyTill anyChar (string "-}"))

comment : Parser Statement
comment =
  choice [ singleLineComment, multiLineComment ]

{-| A parser for Elm statements. -}
statement : OpTable -> Parser Statement
statement ops =
  choice [ importStatement
         , typeAliasDeclaration
         , typeDeclaration
         , portTypeDeclaration
         , portDeclaration ops
         , functionTypeDeclaration
         , functionDeclaration ops
         , comment
         ]
