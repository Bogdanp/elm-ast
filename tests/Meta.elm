module Meta exposing (multiline, oneLine, statements)

import Ast.Common exposing (..)
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (..)
import Helpers exposing (..)
import Test exposing (Test, describe, test)


oneLine : Test
oneLine =
    let
        cases =
            [ ( "a", addMeta 0 0 <| Variable "a" )
            , ( "{a = Just 2}"
              , addMeta 0 0 <|
                    Record
                        [ ( addMeta 0 1 "a"
                          , addMeta 0 5 <|
                                Application
                                    (addMeta 0 5 <| Constructor "Just")
                                    (addMeta 0 10 <| Literal <| Integer 2)
                          )
                        ]
              )
            , ( "let f x = x + 1.2 in f 4.5"
              , addMeta 0 0 <|
                    Let
                        [ ( applicationPattern (variablePattern "f") (variablePattern "x")
                          , addMeta 0 11 <|
                                BinOp (addMeta 0 11 <| Variable "+")
                                    (addMeta 0 10 <| Variable "x")
                                    (addMeta 0 14 <| Literal <| Float 1.2)
                          )
                        ]
                        (addMeta 0 21 <| Application (addMeta 0 21 <| Variable "f") (addMeta 0 23 <| Literal <| Float 4.5))
              )
            , ( "\\(x, y) -> x ++ \":\" ++ y"
              , addMeta 0 0 <|
                    Lambda
                        [ tuplePattern
                            [ variablePattern "x"
                            , variablePattern "y"
                            ]
                        ]
                        (addMeta 0 12 <|
                            BinOp (addMeta 0 12 <| Variable "++")
                                (addMeta 0 11 <| Variable "x")
                                (addMeta 0 19 <|
                                    BinOp (addMeta 0 19 <| Variable "++")
                                        (addMeta 0 16 <| Literal <| String ":")
                                        (addMeta 0 23 <| Variable "y")
                                )
                        )
              )
            , ( "Maybe.Just (A.B.C x)"
              , addMeta 0 0 <|
                    Application
                        (addMeta 0 0 <| External [ "Maybe" ] (addMeta 0 6 <| Constructor "Just"))
                        (addMeta 0 12 <|
                            Application
                                (addMeta 0 12 <| External [ "A", "B" ] (addMeta 0 16 <| Constructor "C"))
                                (addMeta 0 18 <| Variable "x")
                        )
              )
            ]
    in
    describe "One line location" <|
        List.map
            (\( input, expectation ) ->
                test ("One line: " ++ input) (\() -> isExpression expectation input)
            )
            cases


multiline : Test
multiline =
    let
        input =
            """
let
    a = { b = 2, c = 3 }
    f x =
        { a | b = x }
in
    f 12
""" |> String.trim

        expectation =
            addMeta 0 0 <|
                Let
                    [ ( variablePattern "a"
                      , addMeta 1 8 <|
                            Record
                                [ ( addMeta 1 10 <| "b"
                                  , addMeta 1 14 <| Literal <| Integer 2
                                  )
                                , ( addMeta 1 17 <| "c"
                                  , addMeta 1 21 <| Literal <| Integer 3
                                  )
                                ]
                      )
                    , ( applicationPattern (variablePattern "f") (variablePattern "x")
                      , addMeta 3 8 <|
                            RecordUpdate (addMeta 3 10 <| "a")
                                [ ( addMeta 3 14 <| "b"
                                  , addMeta 3 18 <| Variable "x"
                                  )
                                ]
                      )
                    ]
                    (addMeta 5 4 <|
                        Application
                            (addMeta 5 4 <| Variable "f")
                            (addMeta 5 6 <| Literal <| Integer 12)
                    )
    in
    describe "Multiline location"
        [ test "Multiple lines" <|
            \() -> isExpression expectation input
        ]


statements : Test
statements =
    let
        input =
            """
module A exposing (f)

import Html exposing (text)

f : String -> Html msg
f s = text <| s ++ s
""" |> String.trim

        expectations =
            [ addMeta 0 0 <|
                ModuleDeclaration [ "A" ]
                    (SubsetExport [ FunctionExport "f" ])
            , addMeta 2 0 <|
                ImportStatement [ "Html" ] Nothing <|
                    Just (SubsetExport [ FunctionExport "text" ])
            , addMeta 4 0 <|
                FunctionTypeDeclaration "f" <|
                    TypeApplication
                        (TypeConstructor [ "String" ] [])
                        (TypeConstructor [ "Html" ] [ TypeVariable "msg" ])
            , addMeta 5 0 <|
                FunctionDeclaration
                    (applicationPattern (variablePattern "f")
                        (variablePattern "s")
                    )
                <|
                    addMeta 5 10 <|
                        BinOp (addMeta 5 10 <| Variable "<|")
                            (addMeta 5 6 <| Variable "text")
                        <|
                            addMeta 5 15 <|
                                BinOp (addMeta 5 15 <| Variable "++")
                                    (addMeta 5 14 <| Variable "s")
                                    (addMeta 5 19 <| Variable "s")
            ]
    in
    describe "Statements location"
        [ test "full module" <| \() -> areStatements expectations input ]
