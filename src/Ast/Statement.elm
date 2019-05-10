module Ast.Statement exposing
    ( ExportSet(..), Type(..), Statement, StatementBase(..)
    , statement, statements, infixStatements, opTable
    )

{-| This module exposes parsers for Elm statements.


# Types

@docs ExportSet, Type, Statement, StatementBase


# Parsers

@docs statement, statements, infixStatements, opTable

-}

import Ast.BinOp exposing (Assoc(..), OpTable)
import Ast.Common exposing (..)
import Ast.Expression exposing (Expression, MExp, expression, term)
import Ast.Helpers exposing (..)
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Dict
import String


{-| Representations for modules' exports.
-}
type ExportSet
    = AllExport
    | SubsetExport (List ExportSet)
    | FunctionExport Name
    | TypeExport Name (Maybe ExportSet)


{-| Representations for Elm's type syntax.
-}
type Type
    = TypeConstructor QualifiedType (List Type)
    | TypeVariable Name
    | TypeRecordConstructor Type (List ( Name, Type ))
    | TypeRecord (List ( Name, Type ))
    | TypeTuple (List Type)
    | TypeApplication Type Type


{-| Representations for Elm's statements.
-}
type StatementBase
    = ModuleDeclaration ModuleName ExportSet
    | PortModuleDeclaration ModuleName ExportSet
    | EffectModuleDeclaration ModuleName (List ( Name, Name )) ExportSet
    | ImportStatement ModuleName (Maybe Alias) (Maybe ExportSet)
    | TypeAliasDeclaration Type Type
    | TypeDeclaration Type (List Type)
    | PortTypeDeclaration Name Type
    | PortDeclaration Name (List Name) MExp
    | FunctionTypeDeclaration Name Type
    | FunctionDeclaration Name (List MExp) MExp
    | InfixDeclaration Assoc Int Name
    | Comment String


{-| Statement with position in code
-}
type alias Statement =
    WithMeta StatementBase {}



-- Exports
-- -------


allExport : Parser s ExportSet
allExport =
    AllExport <$ symbol ".."


functionExport : Parser s ExportSet
functionExport =
    FunctionExport <$> choice [ functionName, parens operator ]


constructorSubsetExports : Parser s ExportSet
constructorSubsetExports =
    SubsetExport <$> commaSeparated (FunctionExport <$> upName)


constructorExports : Parser s (Maybe ExportSet)
constructorExports =
    maybe <|
        parens <|
            choice
                [ allExport
                , constructorSubsetExports
                ]


typeExport : Parser s ExportSet
typeExport =
    TypeExport <$> (upName <* spaces) <*> constructorExports


subsetExport : Parser s ExportSet
subsetExport =
    SubsetExport
        <$> commaSeparated (functionExport |> or typeExport)


exports : Parser s ExportSet
exports =
    parens <| choice [ allExport, subsetExport ]



-- Types
-- -----


typeVariable : Parser s Type
typeVariable =
    TypeVariable <$> regex "[a-z]+(\\w|_)*"


typeConstant : Parser s Type
typeConstant =
    TypeConstructor <$> sepBy1 (string ".") upName <*> succeed []


typeApplication : Parser s (Type -> Type -> Type)
typeApplication =
    TypeApplication <$ symbol "->"


typeTuple : Parser s Type
typeTuple =
    lazy <|
        \() ->
            TypeTuple <$> parens (commaSeparated_ type_)


typeRecordPair : Parser s ( Name, Type )
typeRecordPair =
    lazy <|
        \() ->
            (,) <$> (loName <* symbol ":") <*> typeAnnotation


typeRecordPairs : Parser s (List ( Name, Type ))
typeRecordPairs =
    lazy <|
        \() ->
            commaSeparated_ typeRecordPair


typeRecordConstructor : Parser s Type
typeRecordConstructor =
    lazy <|
        \() ->
            braces <|
                TypeRecordConstructor
                    <$> between_ spaces typeVariable
                    <*> (symbol "|" *> typeRecordPairs)


typeRecord : Parser s Type
typeRecord =
    lazy <|
        \() ->
            braces <|
                TypeRecord
                    <$> typeRecordPairs


typeParameter : Parser s Type
typeParameter =
    lazy <|
        \() ->
            between_ (or (spaces *> newline *> spaces_) spaces) <|
                choice
                    [ typeVariable
                    , typeConstant
                    , typeRecordConstructor
                    , typeRecord
                    , typeTuple
                    , parens typeAnnotation
                    ]


typeConstructor : Parser s Type
typeConstructor =
    lazy <|
        \() ->
            TypeConstructor <$> sepBy1 (string ".") upName <*> many typeParameter


type_ : Parser s Type
type_ =
    lazy <|
        \() ->
            between_ spaces <|
                choice
                    [ typeConstructor
                    , typeVariable
                    , typeRecordConstructor
                    , typeRecord
                    , typeTuple
                    , parens typeAnnotation
                    ]


typeAnnotation : Parser s Type
typeAnnotation =
    lazy <|
        \() ->
            type_ |> chainr typeApplication



-- Modules
-- -------


portModuleDeclaration : Parser s Statement
portModuleDeclaration =
    withMeta <|
        PortModuleDeclaration
            <$> (initialSymbol "port" *> symbol "module" *> moduleName)
            <*> (symbol "exposing" *> exports)


effectModuleDeclaration : Parser s Statement
effectModuleDeclaration =
    withMeta <|
        EffectModuleDeclaration
            <$> (initialSymbol "effect" *> symbol "module" *> moduleName)
            <*> (symbol "where" *> braces (commaSeparated ((,) <$> loName <*> (symbol "=" *> upName))))
            <*> (symbol "exposing" *> exports)


moduleDeclaration : Parser s Statement
moduleDeclaration =
    withMeta <|
        ModuleDeclaration
            <$> (initialSymbol "module" *> moduleName)
            <*> (symbol "exposing" *> exports)



-- Imports
-- -------


importStatement : Parser s Statement
importStatement =
    withMeta <|
        ImportStatement
            <$> (initialSymbol "import" *> moduleName)
            <*> maybe (symbol "as" *> upName)
            <*> maybe (symbol "exposing" *> exports)



-- Type declarations
-- -----------------


typeAliasDeclaration : Parser s Statement
typeAliasDeclaration =
    withMeta <|
        TypeAliasDeclaration
            <$> (initialSymbol "type" *> symbol "alias" *> type_)
            <*> (whitespace *> symbol "=" *> typeAnnotation)


typeDeclaration : Parser s Statement
typeDeclaration =
    withMeta <|
        TypeDeclaration
            <$> (initialSymbol "type" *> type_)
            <*> (whitespace *> symbol "=" *> sepBy1 (symbol "|") (between_ whitespace typeConstructor))



-- Ports
-- -----


portTypeDeclaration : Parser s Statement
portTypeDeclaration =
    withMeta <|
        PortTypeDeclaration
            <$> (initialSymbol "port" *> loName)
            <*> (symbol ":" *> typeAnnotation)


portDeclaration : OpTable -> Parser s Statement
portDeclaration ops =
    withMeta <|
        PortDeclaration
            <$> (initialSymbol "port" *> loName)
            <*> (many <| between_ spaces loName)
            <*> (symbol "=" *> expression ops)



-- Functions
-- ---------


functionTypeDeclaration : Parser s Statement
functionTypeDeclaration =
    withMeta <|
        FunctionTypeDeclaration
            <$> (choice [ loName, parens operator ] <* symbol ":")
            <*> typeAnnotation


functionDeclaration : OpTable -> Parser s Statement
functionDeclaration ops =
    withMeta <|
        FunctionDeclaration
            <$> choice [ loName, parens operator ]
            <*> many (between_ whitespace <| term ops)
            <*> (symbol "=" *> whitespace *> expression ops)



-- Infix declarations
-- ------------------


infixDeclaration : Parser s Statement
infixDeclaration =
    withMeta <|
        InfixDeclaration
            <$> choice
                    [ L <$ initialSymbol "infixl"
                    , R <$ initialSymbol "infixr"
                    , N <$ initialSymbol "infix"
                    ]
            <*> (spaces *> Combine.Num.int)
            <*> (spaces *> (loName <|> operator))



-- Comments
-- --------


singleLineComment : Parser s Statement
singleLineComment =
    withMeta <|
        Comment
            <$> (string "--" *> regex ".*" <* whitespace)


multiLineComment : Parser s Statement
multiLineComment =
    withMeta <|
        (Comment << String.fromList)
            <$> (string "{-" *> manyTill anyChar (string "-}"))


comment : Parser s Statement
comment =
    singleLineComment <|> multiLineComment


{-| A parser for stand-alone Elm statements.
-}
statement : OpTable -> Parser s Statement
statement ops =
    lazy <|
        \() ->
            choice
                [ portModuleDeclaration
                , effectModuleDeclaration
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


{-| A parser for a series of Elm statements.
-}
statements : OpTable -> Parser s (List Statement)
statements ops =
    manyTill (or whitespace spaces *> statement ops <* or whitespace spaces) end


{-| A scanner for infix statements. This is useful for performing a
first pass over a module to find all of the infix declarations in it.
-}
infixStatements : Parser s (List Statement)
infixStatements =
    let
        statements =
            many
                (choice
                    [ Just <$> infixDeclaration
                    , Nothing <$ regex ".*"
                    ]
                    <* whitespace
                )
                <* end
    in
    statements
        >>= (\xs ->
                succeed <| List.filterMap identity xs
            )


{-| A scanner that returns an updated OpTable based on the infix
declarations in the input.
-}
opTable : OpTable -> Parser s OpTable
opTable ops =
    let
        collect ( s, _ ) d =
            case s of
                InfixDeclaration a l n ->
                    Dict.insert n ( a, l ) d

                _ ->
                    Debug.crash "impossible"
    in
    infixStatements
        >>= (\xs ->
                succeed <| List.foldr collect ops xs
            )
