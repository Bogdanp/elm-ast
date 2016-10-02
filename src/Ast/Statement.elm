module Ast.Statement exposing
    ( ExportSet(..)
    , Type(..)
    , Statement(..)
    , statement
    , statements
    , infixStatements
    , opTable )

{-| This module exposes parsers for Elm statements.

# Types
@docs ExportSet, Type, Statement

# Parsers
@docs statement, statements, infixStatements, opTable

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num
import Dict
import String

import Ast.BinOp exposing (Assoc(..), OpTable)
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
  = TypeConstructor QualifiedType (List Type)
  | TypeVariable Name
  | TypeRecordConstructor Type (List (Name, Type))
  | TypeRecord (List (Name, Type))
  | TypeTuple (List Type)
  | TypeApplication Type Type

{-| Representations for Elm's statements. -}
type Statement
  = ModuleDeclaration ModuleName ExportSet
  | PortModuleDeclaration ModuleName ExportSet
  | ImportStatement ModuleName (Maybe Alias) (Maybe ExportSet)
  | TypeAliasDeclaration Type Type
  | TypeDeclaration Type (List Type)
  | PortTypeDeclaration Name Type
  | PortDeclaration Name (List Name) Expression
  | FunctionTypeDeclaration Name Type
  | FunctionDeclaration Name (List Name) Expression
  | InfixDeclaration Assoc Int Name
  | Comment String


-- Exports
-- -------
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

exports : Parser ExportSet
exports =
  parens <| choice [ allExport, subsetExport ]


-- Types
-- -----
typeVariable : Parser Type
typeVariable =
  TypeVariable <$> regex "[a-z]+"

typeConstant : Parser Type
typeConstant =
  TypeConstructor <$> sepBy1 (string ".") upName <*> succeed []

typeApplication : Parser (Type -> Type -> Type)
typeApplication =
  TypeApplication <$ symbol "->"

typeTuple : Parser Type
typeTuple =
  rec <| \() ->
    TypeTuple <$> parens (commaSeparated' type')

typeRecordPair : Parser (Name, Type)
typeRecordPair =
  rec <| \() ->
    (,) <$> (loName <* symbol ":") <*> typeAnnotation

typeRecordPairs : Parser (List (Name, Type))
typeRecordPairs =
  rec <| \() ->
    commaSeparated' typeRecordPair

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
    TypeConstructor <$> sepBy1 (string ".") upName <*> many typeParameter

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
    type' `chainr` typeApplication


-- Modules
-- -------
portModuleDeclaration : Parser Statement
portModuleDeclaration =
  PortModuleDeclaration
    <$> (initialSymbol "port" *> symbol "module" *> moduleName)
    <*> (symbol "exposing" *> exports)

moduleDeclaration : Parser Statement
moduleDeclaration =
  ModuleDeclaration
    <$> (initialSymbol "module" *> moduleName)
    <*> (symbol "exposing" *> exports)


-- Imports
-- -------
importStatement : Parser Statement
importStatement =
  ImportStatement
    <$> (initialSymbol "import" *> moduleName)
    <*> maybe (symbol "as" *> upName)
    <*> maybe (symbol "exposing" *> exports)


-- Type declarations
-- -----------------
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


-- Ports
-- -----

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


-- Functions
-- ---------
functionTypeDeclaration : Parser Statement
functionTypeDeclaration =
  FunctionTypeDeclaration <$> (loName <* symbol ":") <*> typeAnnotation

functionDeclaration : OpTable -> Parser Statement
functionDeclaration ops =
  FunctionDeclaration
    <$> loName
    <*> (many (between' whitespace loName))
    <*> (symbol "=" *> whitespace *> expression ops)


-- Infix declarations
-- ------------------
infixDeclaration : Parser Statement
infixDeclaration =
  InfixDeclaration
    <$> choice [ L <$ initialSymbol "infixl"
               , R <$ initialSymbol "infixr"
               , N <$ initialSymbol "infix"
               ]
    <*> (spaces *> Combine.Num.int)
    <*> (spaces *> (loName <|> operator))


-- Comments
-- --------
singleLineComment : Parser Statement
singleLineComment =
  Comment <$> (string "--" *> regex ".*" <* whitespace)

multiLineComment : Parser Statement
multiLineComment =
  (Comment << String.fromList) <$> (string "{-" *> manyTill anyChar (string "-}"))

comment : Parser Statement
comment =
  singleLineComment <|> multiLineComment


{-| A parser for stand-alone Elm statements. -}
statement : OpTable -> Parser Statement
statement ops =
  choice [ portModuleDeclaration
         , moduleDeclaration
         , importStatement
         , typeAliasDeclaration
         , typeDeclaration
         , portTypeDeclaration
         , portDeclaration ops
         , functionTypeDeclaration
         , functionDeclaration ops
         , infixDeclaration
         , comment
         ]

{-| A parser for a series of Elm statements. -}
statements : OpTable -> Parser (List Statement)
statements ops =
  manyTill (whitespace *> statement ops <* whitespace) end

{-| A scanner for infix statements. This is useful for performing a
first pass over a module to find all of the infix declarations in it.
-}
infixStatements : Parser (List Statement)
infixStatements =
  let
    statements =
      many ( choice [ Just    <$> infixDeclaration
                    , Nothing <$  regex ".*"
                    ] <* whitespace ) <* end
  in
    statements `andThen` \xs ->
      succeed <| List.filterMap identity xs

{-| A scanner that returns an updated OpTable based on the infix
declarations in the input. -}
opTable : OpTable -> Parser OpTable
opTable ops =
  let
    collect s d =
      case s of
        InfixDeclaration a l n ->
          Dict.insert n (a, l) d

        _ ->
          Debug.crash "impossible"
  in
    infixStatements `andThen` \xs ->
      succeed <| List.foldr collect ops xs
