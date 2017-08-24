module Multiline exposing (all)

import Ast exposing (parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Expect exposing (..)
import String
import Test exposing (describe, test, Test)


var a =
    Variable [ a ]


is : Expression -> String -> Expectation
is e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e r

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


all : Test
all =
    Test.only <|
        describe "Multiline performance"
            [ test "Application" <|
                \() ->
                    "fn\n a\n b\n c\n d\n e\n f\n g\n h\n i\n j\n k"
                        |> is
                            (Application
                                (Application
                                    (Application
                                        (Application
                                            (Application
                                                (Application
                                                    (Application
                                                        (Application
                                                            (Application
                                                                (Application
                                                                    (Application
                                                                        (var "fn")
                                                                        (var "a")
                                                                    )
                                                                    (var "b")
                                                                )
                                                                (var "c")
                                                            )
                                                            (var "d")
                                                        )
                                                        (var "e")
                                                    )
                                                    (var "f")
                                                )
                                                (var "g")
                                            )
                                            (var "h")
                                        )
                                        (var "i")
                                    )
                                    (var "j")
                                )
                                (var "k")
                            )
            ]
