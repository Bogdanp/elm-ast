module Multiline exposing (application)

import Ast exposing (parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Test exposing (describe, test, skip, Test)
import Helpers exposing (isExpression, var)


application : Test
application =
    skip <|
        describe "Multiline performance"
            [ test "Application" <|
                \() ->
                    "fn\n a\n b\n c\n d\n e\n f\n g\n h\n i\n j\n k"
                        |> isExpression
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
                                                                        (var "fn" { line = 1, column = 0 })
                                                                        (var "a" { line = 1, column = 0 })
                                                                        { line = 1, column = 0 }
                                                                    )
                                                                    (var "b" { line = 2, column = 0 })
                                                                    { line = 2, column = 0 }
                                                                )
                                                                (var "c" { line = 3, column = 0 })
                                                                { line = 3, column = 0 }
                                                            )
                                                            (var "d" { line = 4, column = 0 })
                                                            { line = 4, column = 0 }
                                                        )
                                                        (var "e" { line = 5, column = 0 })
                                                        { line = 5, column = 0 }
                                                    )
                                                    (var "f" { line = 6, column = 0 })
                                                    { line = 6, column = 0 }
                                                )
                                                (var "g" { line = 7, column = 0 })
                                                { line = 7, column = 0 }
                                            )
                                            (var "h" { line = 8, column = 0 })
                                            { line = 8, column = 0 }
                                        )
                                        (var "i" { line = 9, column = 0 })
                                        { line = 9, column = 0 }
                                    )
                                    (var "j" { line = 10, column = 0 })
                                    { line = 10, column = 0 }
                                )
                                (var "k" { line = 12, column = 1 })
                                { line = 12, column = 0 }
                            )
            ]
