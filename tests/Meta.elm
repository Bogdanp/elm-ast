module Meta exposing (..)

import Ast.Expression exposing (Expression(..), addMeta)
import Helpers exposing (..)
import Test exposing (Test, describe, test)

oneLine : Test
oneLine =
    let
        cases =
            [ ( "a", addMeta 1 0 <| Variable [ "a" ] )
            , ( "{a = Just 2}"
              , addMeta 1 0 <|
                    Record
                        [ ( addMeta 1 1 "a"
                          , addMeta 1 5 <|
                                Application
                                    (addMeta 1 5 <| Variable [ "Just" ])
                                    (addMeta 1 10 <| Integer 2)
                          )
                        ]
              )
            , ( "let f x = x + 1.2 in f 4.5"
              , addMeta 1 0 <|
                    Let
                        [ ( addMeta 1 4 <|
                                Application (addMeta 1 4 <| Variable [ "f" ])
                                    (addMeta 1 6 <| Variable [ "x" ])
                          , addMeta 1 10 <|
                                BinOp (addMeta 1 12 <| Variable [ "+" ])
                                    (addMeta 1 10 <| Variable [ "x" ])
                                    (addMeta 1 14 <| Float 1.2)
                          )
                        ]
                        (addMeta 1 21 <| Application (addMeta 1 21 <| Variable [ "f" ]) (addMeta 1 23 <| Float 4.5))
              )
            , ( "\\(x, y) -> x ++ \":\" ++ y"
              , addMeta 1 0 <|
                    Lambda
                        [ addMeta 1 1 <|
                            Tuple
                                [ addMeta 1 2 <| Variable [ "x" ]
                                , addMeta 1 5 <| Variable [ "y" ]
                                ]
                        ]
                        (addMeta 1 12 <|
                            BinOp (addMeta 1 12 <| Variable [ "++" ])
                                (addMeta 1 11 <| Variable [ "x" ])
                                (addMeta 1 19 <|
                                    BinOp (addMeta 1 19 <| Variable [ "++" ])
                                        (addMeta 1 16 <| String ":")
                                        (addMeta 1 23 <| Variable [ "y" ])
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
"""

        expectation =
            addMeta 1 0 <|
                Let
                    [ ( addMeta 2 4 <| Variable [ "a" ]
                      , addMeta 2 8 <|
                            Record
                                [ ( addMeta 2 10 <| "b"
                                  , addMeta 2 14 <| Integer 2
                                  )
                                , ( addMeta 2 17 <| "c"
                                  , addMeta 2 21 <| Integer 3
                                  )
                                ]
                      )
                    , ( addMeta 3 4 <|
                            Application
                                (addMeta 3 4 <| Variable [ "f" ])
                                (addMeta 3 6 <| Variable [ "x" ])
                      , addMeta 4 8 <|
                            RecordUpdate (addMeta 4 10 <| "a")
                                [ ( addMeta 4 14 <| "b"
                                  , addMeta 4 18 <| Variable [ "x" ]
                                  )
                                ]
                      )
                    ]
                    (addMeta 6 8 <|
                        Application
                            (addMeta 6 4 <| Variable [ "f" ])
                            (addMeta 6 8 <| Integer 12)
                    )
    in
    describe "Multiline location"
        [ test "Multiple lines" <|
            \() -> isExpression expectation input
        ]
