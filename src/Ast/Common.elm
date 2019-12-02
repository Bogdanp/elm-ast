module Ast.Common exposing
    ( Alias, Column, Line, Located, MName, ModuleName, Name, QualifiedType, WithMeta, Literal(..), Pattern(..), MPattern
    , addMeta, dropMeta, withMeta
    )

{-| This module exposes types and helpers common for statement and expression.


# Types

@docs Alias, Column, Line, Located, MName, ModuleName, Name, QualifiedType, WithMeta, Literal, Pattern, MPattern


# helpers

@docs addMeta, dropMeta, withMeta

-}

import Combine exposing (..)


{-| Represents a line of source in which an expression is located
-}
type alias Line =
    Int


{-| Represents a column of source in which an expression is located
-}
type alias Column =
    Int


{-| Part of metadata of an entity that can be associated with a line and column of source
-}
type alias Located x =
    { x | line : Int, column : Int }


{-| An entity that can be associated with a line and column of source
-}
type alias WithMeta x m =
    ( x, Located m )


{-| Name for a variable
-}
type alias Name =
    String


{-| Name for a variable with its location in source
-}
type alias MName =
    WithMeta Name {}


{-| Name with more qualifiers
-}
type alias QualifiedType =
    List Name


{-| Name for a module
-}
type alias ModuleName =
    List String


{-| An alias for an entity
-}
type alias Alias =
    String


{-| Simple literal patterns
-}
type Literal
    = Character Char
    | String String
    | Integer Int
    | Float Float


{-| Pattern to match
-}
type Pattern
    = PWild
    | PVariable Name
    | PConstructor Name
    | PLiteral Literal
    | PTuple (List MPattern)
    | PCons MPattern MPattern
    | PList (List MPattern)
    | PRecord (List MName)
    | PAs MPattern Name
    | PApplication MPattern MPattern


{-| Pattern with location
-}
type alias MPattern =
    WithMeta Pattern {}


{-| Helper adding metadata to an entity
-}
addMeta : Line -> Column -> x -> WithMeta x {}
addMeta l c e =
    ( e, { line = l, column = c } )


{-| Create metadata for an entity from parser's location
-}
withMeta : Parser s x -> Parser s (WithMeta x {})
withMeta p =
    withLocation <| \a -> map (\x -> addMeta a.line a.column x) p


{-| Extract only an entity dropping its metadata
-}
dropMeta : WithMeta a {} -> a
dropMeta ( e, _ ) =
    e
