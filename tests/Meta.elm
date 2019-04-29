module Meta exposing (multiline, oneLine)

import Ast.Expression exposing (Expression(..), addMeta)
import Helpers exposing (..)
import Test exposing (Test, describe, test)


oneLine : Test
oneLine =
    let
        cases =
            [ ( "a", addMeta 1 0 "" <| Variable [ "a" ] )
            , ( "{a = Just 2}"
              , addMeta 1 0 "" <|
                    Record
                        [ ( addMeta 1 1 "" "a"
                          , addMeta 1 5 "" <|
                                Application
                                    (addMeta 1 5 "" <| Variable [ "Just" ])
                                    (addMeta 1 0 "" <| Integer 2)
                          )
                        ]
              )
            , ( "let f x = x + 1.2 in f 4.5"
              , addMeta 1 0 "" <|
                    Let
                        [ ( addMeta 1 4 "" <|
                                Application (addMeta 1 4 "" <| Variable [ "f" ])
                                    (addMeta 1 6 "" <| Variable [ "x" ])
                          , addMeta 1 0 "" <|
                                BinOp (addMeta 1 2 "" <| Variable [ "+" ])
                                    (addMeta 1 0 "" <| Variable [ "x" ])
                                    (addMeta 1 4 "" <| Float 1.2)
                          )
                        ]
                        (addMeta 1 1 "" <| Application (addMeta 1 1 "" <| Variable [ "f" ]) (addMeta 1 3 "" <| Float 4.5))
              )
            , ( "\\(x, y) -> x ++ \":\" ++ y"
              , addMeta 1 0 "" <|
                    Lambda
                        [ addMeta 1 1 "" <|
                            Tuple
                                [ addMeta 1 2 "" <| Variable [ "x" ]
                                , addMeta 1 5 "" <| Variable [ "y" ]
                                ]
                        ]
                        (addMeta 1 2 "" <|
                            BinOp (addMeta 1 2 "" <| Variable [ "++" ])
                                (addMeta 1 1 "" <| Variable [ "x" ])
                                (addMeta 1 9 "" <|
                                    BinOp (addMeta 1 9 "" <| Variable [ "++" ])
                                        (addMeta 1 6 "" <| String ":")
                                        (addMeta 1 3 "" <| Variable [ "y" ])
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
            addMeta 1 0 "" <|
                Let
                    [ ( addMeta 2 4 "" <| Variable [ "a" ]
                      , addMeta 2 8 "" <|
                            Record
                                [ ( addMeta 2 0 "" <| "b"
                                  , addMeta 2 4 "" <| Integer 2
                                  )
                                , ( addMeta 2 7 "" <| "c"
                                  , addMeta 2 1 "" <| Integer 3
                                  )
                                ]
                      )
                    , ( addMeta 3 4 "" <|
                            Application
                                (addMeta 3 4 "" <| Variable [ "f" ])
                                (addMeta 3 6 "" <| Variable [ "x" ])
                      , addMeta 4 8 "" <|
                            RecordUpdate (addMeta 4 0 "" <| "a")
                                [ ( addMeta 4 4 "" <| "b"
                                  , addMeta 4 8 "" <| Variable [ "x" ]
                                  )
                                ]
                      )
                    ]
                    (addMeta 6 8 "" <|
                        Application
                            (addMeta 6 4 "" <| Variable [ "f" ])
                            (addMeta 6 8 "" <| Integer 12)
                    )
    in
    describe "Multiline location"
        [ test "Multiple lines" <|
            \() -> isExpression expectation input
        ]
