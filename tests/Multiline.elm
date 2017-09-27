module Multiline exposing (application)

import Ast exposing (parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Test exposing (describe, test, Test)
import Helpers exposing (isExpression, var)


application : Test
application =
    describe "Multiline performance"
        [ test "Application" <|
            \() ->
                "fn\n a\n b\n c\n d\n e\n f\n g\n h\n i\n j\n k"
                    |> isExpression (makeApplication "fn" ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])

        ]


makeApplication : String -> List String -> Expression
makeApplication name args =
    let
        line n =
            { line = n + 1, column = 0 }

        nextArgs ( index, name ) acc =
            Application acc (var name <| line index) (line index)
    in
        args
            |> List.indexedMap (,)
            |> List.foldl nextArgs (var name (line 0))
