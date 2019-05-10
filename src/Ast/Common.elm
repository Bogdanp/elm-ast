module Ast.Common exposing (Alias, Column, Line, Located, MName, ModuleName, Name, QualifiedType, WithMeta, addMeta, dropMeta, withMeta)

import Combine exposing (..)


type alias Line =
    Int


type alias Column =
    Int


type alias Located x =
    { x | line : Int, column : Int }


type alias WithMeta x m =
    ( x, Located m )


type alias Name =
    String


type alias MName =
    WithMeta Name {}


type alias QualifiedType =
    List Name


type alias ModuleName =
    List String


type alias Alias =
    String


addMeta : Line -> Column -> x -> WithMeta x {}
addMeta l c e =
    ( e, { line = l, column = c } )


withMeta : Parser s x -> Parser s (WithMeta x {})
withMeta p =
    withLocation (\a -> (\x -> addMeta a.line a.column x) <$> p)


dropMeta : WithMeta a {} -> a
dropMeta ( e, _ ) =
    e
