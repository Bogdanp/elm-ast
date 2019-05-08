module Meta exposing (multiline, oneLine, statements)

import Ast.Expression exposing (Expression(..))
import Ast.Helpers exposing (..)
import Ast.Statement exposing (..)
import Helpers exposing (..)
import Test exposing (Test, describe, test)


oneLine : Test
oneLine =
    let
        cases =
            [ ( "a", addMeta 0 0 <| Variable [ "a" ] )
            , ( "{a = Just 2}"
              , addMeta 0 0 <|
                    Record
                        [ ( addMeta 0 1 "a"
                          , addMeta 0 5 <|
                                Application
                                    (addMeta 0 5 <| Variable [ "Just" ])
                                    (addMeta 0 10 <| Integer 2)
                          )
                        ]
              )
            , ( "let f x = x + 1.2 in f 4.5"
              , addMeta 0 0 <|
                    Let
                        [ ( addMeta 0 4 <|
                                Application (addMeta 0 4 <| Variable [ "f" ])
                                    (addMeta 0 6 <| Variable [ "x" ])
                          , addMeta 0 11 <|
                                BinOp (addMeta 0 11 <| Variable [ "+" ])
                                    (addMeta 0 10 <| Variable [ "x" ])
                                    (addMeta 0 14 <| Float 1.2)
                          )
                        ]
                        (addMeta 0 21 <| Application (addMeta 0 21 <| Variable [ "f" ]) (addMeta 0 23 <| Float 4.5))
              )
            , ( "\\(x, y) -> x ++ \":\" ++ y"
              , addMeta 0 0 <|
                    Lambda
                        [ addMeta 0 1 <|
                            Tuple
                                [ addMeta 0 2 <| Variable [ "x" ]
                                , addMeta 0 5 <| Variable [ "y" ]
                                ]
                        ]
                        (addMeta 0 12 <|
                            BinOp (addMeta 0 12 <| Variable [ "++" ])
                                (addMeta 0 11 <| Variable [ "x" ])
                                (addMeta 0 19 <|
                                    BinOp (addMeta 0 19 <| Variable [ "++" ])
                                        (addMeta 0 16 <| String ":")
                                        (addMeta 0 23 <| Variable [ "y" ])
                                )
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
                    [ ( addMeta 1 4 <| Variable [ "a" ]
                      , addMeta 1 8 <|
                            Record
                                [ ( addMeta 1 10 <| "b"
                                  , addMeta 1 14 <| Integer 2
                                  )
                                , ( addMeta 1 17 <| "c"
                                  , addMeta 1 21 <| Integer 3
                                  )
                                ]
                      )
                    , ( addMeta 2 4 <|
                            Application
                                (addMeta 2 4 <| Variable [ "f" ])
                                (addMeta 2 6 <| Variable [ "x" ])
                      , addMeta 3 8 <|
                            RecordUpdate (addMeta 3 10 <| "a")
                                [ ( addMeta 3 14 <| "b"
                                  , addMeta 3 18 <| Variable [ "x" ]
                                  )
                                ]
                      )
                    ]
                    (addMeta 5 4 <|
                        Application
                            (addMeta 5 4 <| Variable [ "f" ])
                            (addMeta 5 6 <| Integer 12)
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
                FunctionDeclaration "f"
                    [ addMeta 5 2 <| Variable [ "s" ] ]
                <|
                    addMeta 5 10 <|
                        BinOp (addMeta 5 10 <| Variable [ "<|" ])
                            (addMeta 5 6 <| Variable [ "text" ])
                        <|
                            addMeta 5 15 <|
                                BinOp (addMeta 5 15 <| Variable [ "++" ])
                                    (addMeta 5 14 <| Variable [ "s" ])
                                    (addMeta 5 19 <| Variable [ "s" ])
            ]
    in
    describe "Statements location"
        [ test "full module" <| \() -> areStatements expectations input ]
