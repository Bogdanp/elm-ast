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
import Ast.Pattern exposing (pattern)
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
    | FunctionDeclaration MPattern MExp
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
    onsuccess AllExport <| symbol ".."


functionExport : Parser s ExportSet
functionExport =
    map FunctionExport <| choice [ functionName, parens operator ]


constructorSubsetExports : Parser s ExportSet
constructorSubsetExports =
    map SubsetExport <| commaSeparated <| map FunctionExport upName


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
    map TypeExport (upName |> ignore spaces) |> andMap constructorExports


subsetExport : Parser s ExportSet
subsetExport =
    map SubsetExport <| commaSeparated (functionExport |> or typeExport)


exports : Parser s ExportSet
exports =
    parens <| choice [ allExport, subsetExport ]



-- Types
-- -----


typeVariable : Parser s Type
typeVariable =
    map TypeVariable (regex "[a-z]+(\\w|_)*")


typeConstant : Parser s Type
typeConstant =
    map TypeConstructor (sepBy1 (string ".") upName)
        |> andMap (succeed [])


typeApplication : Parser s (Type -> Type -> Type)
typeApplication =
    onsuccess TypeApplication <| symbol "->"


typeTuple : Parser s Type
typeTuple =
    lazy <|
        \() ->
            map TypeTuple <| parens (commaSeparated_ type_)


typeRecordPair : Parser s ( Name, Type )
typeRecordPair =
    lazy <|
        \() ->
            map Tuple.pair (loName |> ignore (symbol ":"))
                |> andMap typeAnnotation


typeRecordPairs : Parser s (List ( Name, Type ))
typeRecordPairs =
    lazy <|
        \() ->
            commaSeparated_ typeRecordPair


typeRecordConstructor : Parser s Type
typeRecordConstructor =
    lazy <|
        \() ->
            map TypeRecordConstructor (between_ spaces typeVariable)
                |> andMap (typeRecordPairs |> ignore (symbol "|"))
                |> braces


typeRecord : Parser s Type
typeRecord =
    lazy <|
        \() ->
            braces <| map TypeRecord typeRecordPairs


typeParameter : Parser s Type
typeParameter =
    lazy <|
        \() ->
            between_ (or (spaces |> keep newline |> keep spaces_) spaces) <|
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
            map TypeConstructor (sepBy1 (string ".") upName)
                |> andMap (many typeParameter)


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
    map PortModuleDeclaration (initialSymbol "port" |> keep (symbol "module") |> keep moduleName)
        |> andMap (symbol "exposing" |> keep exports)
        |> withMeta


effectModuleDeclaration : Parser s Statement
effectModuleDeclaration =
    map EffectModuleDeclaration (initialSymbol "effect" |> keep (symbol "module") |> keep moduleName)
        |> andMap
            (symbol "where"
                |> keep
                    (braces
                        (commaSeparated (map Tuple.pair loName |> andMap (symbol "=" |> keep upName)))
                    )
            )
        |> andMap (symbol "exposing" |> keep exports)
        |> withMeta


moduleDeclaration : Parser s Statement
moduleDeclaration =
    map ModuleDeclaration (initialSymbol "module" |> keep moduleName)
        |> andMap (symbol "exposing" |> keep exports)
        |> withMeta



-- Imports
-- -------


importStatement : Parser s Statement
importStatement =
    map ImportStatement (initialSymbol "import" |> keep moduleName)
        |> andMap (maybe (symbol "as" |> keep upName))
        |> andMap (maybe (symbol "exposing" |> keep exports))
        |> withMeta



-- Type declarations
-- -----------------


typeAliasDeclaration : Parser s Statement
typeAliasDeclaration =
    map TypeAliasDeclaration (initialSymbol "type" |> keep (symbol "alias") |> keep type_)
        |> andMap (whitespace |> keep (symbol "=") |> keep typeAnnotation)
        |> withMeta


typeDeclaration : Parser s Statement
typeDeclaration =
    map TypeDeclaration (initialSymbol "type" |> keep type_)
        |> andMap (whitespace |> keep (symbol "=") |> keep (sepBy1 (symbol "|") (between_ whitespace typeConstructor)))
        |> withMeta



-- Ports
-- -----


portTypeDeclaration : Parser s Statement
portTypeDeclaration =
    map PortTypeDeclaration (initialSymbol "port" |> keep loName)
        |> andMap (symbol ":" |> keep typeAnnotation)
        |> withMeta


portDeclaration : OpTable -> Parser s Statement
portDeclaration ops =
    map PortDeclaration (initialSymbol "port" |> keep loName)
        |> andMap (many <| between_ spaces loName)
        |> andMap (symbol "=" |> keep (expression ops))
        |> withMeta



-- Functions
-- ---------


functionTypeDeclaration : Parser s Statement
functionTypeDeclaration =
    map FunctionTypeDeclaration (funName |> ignore (symbol ":"))
        |> andMap typeAnnotation
        |> withMeta


functionDeclaration : OpTable -> Parser s Statement
functionDeclaration ops =
    (map FunctionDeclaration pattern
        |> andMap (symbol "=" |> keep whitespace |> keep (expression ops))
        |> withMeta
    )
        |> andThen
            (\(( decl, _ ) as full) ->
                case decl of
                    FunctionDeclaration ( PVariable _, _ ) _ ->
                        succeed full

                    FunctionDeclaration ( PApplication _ _, _ ) _ ->
                        succeed full

                    _ ->
                        fail "wrong pattern in function declaration"
            )



-- Infix declarations
-- ------------------


infixDeclaration : Parser s Statement
infixDeclaration =
    map InfixDeclaration
        (choice
            [ onsuccess L (initialSymbol "infixl")
            , onsuccess R (initialSymbol "infixr")
            , onsuccess N (initialSymbol "infix")
            ]
        )
        |> andMap (spaces |> keep Combine.Num.int)
        |> andMap (spaces |> keep (or loName operator))
        |> withMeta



-- Comments
-- --------


singleLineComment : Parser s Statement
singleLineComment =
    map Comment (string "--" |> keep (regex ".*") |> ignore whitespace)
        |> withMeta


multiLineComment : Parser s Statement
multiLineComment =
    map (Comment << String.fromList)
        (string "{-" |> keep (manyTill anyChar (string "-}")))
        |> withMeta


comment : Parser s Statement
comment =
    or singleLineComment multiLineComment


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
    manyTill
        (or whitespace spaces |> keep (statement ops) |> ignore (or whitespace spaces))
        end


{-| A scanner for infix statements. This is useful for performing a
first pass over a module to find all of the infix declarations in it.
-}
infixStatements : Parser s (List Statement)
infixStatements =
    let
        statements_ =
            many
                (choice
                    [ map Just infixDeclaration
                    , onsuccess Nothing (regex ".*")
                    ]
                    |> ignore whitespace
                )
                |> ignore end
    in
    statements_
        |> andThen
            (\xs ->
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
                    Debug.todo "impossible"
    in
    infixStatements
        |> andThen
            (\xs ->
                succeed <| List.foldr collect ops xs
            )
