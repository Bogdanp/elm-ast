module Expression
    exposing
        ( application
        , caseExpressions
        , characterLiterals
        , expressions
        , floatLiterals
        , intLiterals
        , letExpressions
        , list
        , literals
        , record
        , stringLiterals
        , tuple
        )

import Ast.Expression exposing (..)
import String
import Test exposing (describe, test, Test)
import Helpers exposing (var, fails, isExpression)


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> isExpression (Character 'a' { line = 1, column = 0 })
        , test "newline literal" <|
            \() -> "'\n'" |> isExpression (Character '\n' { line = 1, column = 0 })
        , test "Charcode literals" <|
            \() -> "'\\x23'" |> isExpression (Character '#' { line = 1, column = 0 })
        , test "character literals must contain one character" <|
            \() -> fails "''"
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> isExpression (Integer 0 { line = 1, column = 0 })
        , test "positive literal" <|
            \() -> "+12" |> isExpression (Integer 12 { line = 1, column = 0 })
        , test "negative literal" <|
            \() -> "-12" |> isExpression (Integer -12 { line = 1, column = 0 })
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> isExpression (Float 0.5 { line = 1, column = 0 })
        , test "positive literal" <|
            \() -> "+12.5" |> isExpression (Float 12.5 { line = 1, column = 0 })
        , test "negative literal" <|
            \() -> "-12.5" |> isExpression (Float -12.5 { line = 1, column = 0 })
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> isExpression (String "" { line = 1, column = 0 })
        , test "simple string" <|
            \() -> "\"hello\"" |> isExpression (String "hello" { line = 1, column = 0 })
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> isExpression (String "hello, \\\"world\\\"" { line = 1, column = 0 })
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> isExpression (String "" { line = 1, column = 0 })
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> isExpression (String "hello\nworld" { line = 1, column = 0 })
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> isExpression (String "\\\\" { line = 1, column = 0 })
        ]


literals : Test
literals =
    describe "Literals"
        [ characterLiterals
        , intLiterals
        , floatLiterals
        , stringLiterals
        ]


letExpressions : Test
letExpressions =
    describe "Let"
        [ test "single binding" <|
            \() ->
                "let a = 42 in a"
                    |> isExpression
                        (Let
                            [ ( var "a" { line = 1, column = 4 }
                              , Integer 42 { line = 1, column = 8 }
                              )
                            ]
                            (var "a" { line = 1, column = 14 })
                            { line = 1, column = 0 }
                        )
        , test "bind to _" <|
            \() ->
                "let _ = 42 in 24"
                    |> isExpression
                        (Let
                            [ ( var "_" { line = 1, column = 4 }, Integer 42 { line = 1, column = 8 } ) ]
                            (Integer 24 { line = 1, column = 14 })
                            { line = 1, column = 0 }
                        )
        , test "Can start with a tag name" <|
            \() ->
                "let letter = 1 \n in letter"
                    |> isExpression
                        (Let
                            [ ( var "letter" { line = 1, column = 4 }, Integer 1 { line = 1, column = 13 } ) ]
                            (var "letter" { line = 2, column = 4 })
                            { line = 1, column = 0 }
                        )
        , test "function 1" <|
            \() ->
                """
let
  f x = x + 1
in
  f 4
        """
                    |> isExpression
                        (Let
                            [ ( Application
                                    (var "f" { line = 1, column = 1 })
                                    (var "x" { line = 1, column = 3 })
                                    { line = 1, column = 2 }
                              , (BinOp
                                    (var "+" { line = 1, column = 10 })
                                    (var "x" { line = 1, column = 7 })
                                    (Integer 1 { line = 1, column = 11 })
                                    { line = 1, column = 10 }
                                )
                              )
                            ]
                            (Application
                                (var "f" { line = 4, column = 2 })
                                (Integer 4 { line = 4, column = 4 })
                                { line = 4, column = 3 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "function 2" <|
            \() ->
                """
let
  f x = x + 1
  g x = x + 1
in
  f 4
        """
                    |> isExpression
                        (Let
                            [ ( (Application
                                    (var "f" { line = 1, column = 1 })
                                    (var "x" { line = 1, column = 3 })
                                    { line = 1, column = 2 }
                                )
                              , (BinOp
                                    (var "+" { line = 1, column = 10 })
                                    (var "x" { line = 1, column = 7 })
                                    (Integer 1 { line = 1, column = 11 })
                                    { line = 1, column = 10 }
                                )
                              )
                            , ( Application
                                    (var "g" { line = 2, column = 1 })
                                    (var "x" { line = 2, column = 3 })
                                    { line = 2, column = 2 }
                              , (BinOp
                                    (var "+" { line = 2, column = 10 })
                                    (var "x" { line = 2, column = 7 })
                                    (Integer 1 { line = 2, column = 11 })
                                    { line = 2, column = 10 }
                                )
                              )
                            ]
                            (Application
                                (var "f" { line = 5, column = 2 })
                                (Integer 4 { line = 5, column = 4 })
                                { line = 5, column = 3 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "multiple bindings" <|
            \() ->
                """
let
  a = 42

  b = a + 1
in
  b
            """
                    |> isExpression
                        (Let
                            [ ( var "a" { line = 1, column = 1 }
                              , Integer 42 { line = 1, column = 5 }
                              )
                            , ( var "b" { line = 3, column = 1 }
                              , (BinOp
                                    (var "+" { line = 3, column = 8 })
                                    (var "a" { line = 3, column = 5 })
                                    (Integer 1 { line = 3, column = 9 })
                                    { line = 3, column = 8 }
                                )
                              )
                            ]
                            (var "b" { line = 6, column = 2 })
                            { line = 1, column = 0 }
                        )
        ]


caseExpressions : Test
caseExpressions =
    describe "Case"
        [ test "simple statement" <|
            \() ->
                """
case x of
  Nothing ->
    0

  Just y ->
    y
          """
                    |> isExpression
                        (Case
                            (var "x" { line = 1, column = 5 })
                            [ ( var "Nothing" { line = 1, column = 1 }
                              , Integer 0 { line = 2, column = 3 }
                              )
                            , ( Application
                                    (var "Just" { line = 4, column = 1 })
                                    (var "y" { line = 4, column = 6 })
                                    { line = 4, column = 5 }
                              , (var "y" { line = 6, column = 4 })
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "binding to underscore" <|
            \() ->
                """
case x of
  _ ->
    42
          """
                    |> isExpression
                        (Case
                            (var "x" { line = 1, column = 5 })
                            [ ( var "_" { line = 1, column = 1 }
                              , Integer 42 { line = 3, column = 4 }
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        ]


application : Test
application =
    describe "Application"
        [ test "simple application" <|
            \() ->
                "f a"
                    |> isExpression
                        (Application
                            (var "f" { line = 1, column = 0 })
                            (var "a" { line = 1, column = 2 })
                            { line = 1, column = 1 }
                        )
        , test "curried application" <|
            \() ->
                "f a b"
                    |> isExpression
                        (Application
                            (Application
                                (var "f" { line = 1, column = 0 })
                                (var "a" { line = 1, column = 2 })
                                { line = 1, column = 1 }
                            )
                            (var "b" { line = 1, column = 4 })
                            { line = 1, column = 3 }
                        )
        , test "curried application with parens" <|
            \() ->
                "(f a) b"
                    |> isExpression
                        (Application
                            (Application
                                (var "f" { line = 1, column = 1 })
                                (var "a" { line = 1, column = 3 })
                                { line = 1, column = 2 }
                            )
                            (var "b" { line = 1, column = 6 })
                            { line = 1, column = 5 }
                        )
        , test "multiline application" <|
            \() ->
                "f\n a\n b"
                    |> isExpression
                        (Application
                            (Application
                                (var "f" { line = 1, column = 0 })
                                (var "a" { line = 1, column = 0 })
                                { line = 1, column = 0 }
                            )
                            (var "b" { line = 3, column = 1 })
                            { line = 3, column = 0 }
                        )
        , test "multiline bug" <|
            \() ->
                "f\n (==)"
                    |> isExpression
                        (Application
                            (var "f" { line = 1, column = 0 })
                            (var "==" { line = 2, column = 1 })
                            { line = 2, column = 0 }
                        )
        , test "same multiline bug" <|
            \() ->
                "f\n \"I like the symbol =\""
                    |> isExpression
                        (Application
                            (var "f" { line = 1, column = 0 })
                            (String "I like the symbol =" { line = 2, column = 1 })
                            { line = 2, column = 0 }
                        )
        , test "constructor application" <|
            \() ->
                "Cons a Nil"
                    |> isExpression
                        (Application
                            (Application
                                (var "Cons" { line = 1, column = 0 })
                                (var "a" { line = 1, column = 5 })
                                { line = 1, column = 4 }
                            )
                            (var "Nil" { line = 1, column = 7 })
                            { line = 1, column = 6 }
                        )
        ]


tuple : Test
tuple =
    describe "Tuples"
        [ test "Empty tuple" <|
            \() -> "()" |> isExpression (var "()" { line = 1, column = 0 })
        , test "Simple tuple" <|
            \() ->
                "(a, b)"
                    |> isExpression
                        (Tuple
                            [ (var "a" { line = 1, column = 1 })
                            , (var "b" { line = 1, column = 4 })
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simple tuple with format" <|
            \() ->
                "( a, b )"
                    |> isExpression
                        (Tuple
                            [ (var "a" { line = 1, column = 2 })
                            , (var "b" { line = 1, column = 5 })
                            ]
                            { line = 1, column = 0 }
                        )
        ]


list : Test
list =
    describe "Lists"
        [ test "Empty list" <| \() -> "[]" |> isExpression (List [] { line = 1, column = 0 })
        , test "Simple list" <|
            \() ->
                "[1, 2]"
                    |> isExpression
                        (List
                            [ Integer 1
                                { line = 1, column = 1 }
                            , Integer 2 { line = 1, column = 4 }
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Tuple list" <|
            \() ->
                "[(a, b), (a, b)]"
                    |> isExpression
                        (List
                            [ (Tuple
                                [ (var "a" { line = 1, column = 2 })
                                , (var "b" { line = 1, column = 5 })
                                ]
                                { line = 1, column = 1 }
                              )
                            , (Tuple
                                [ (var "a" { line = 1, column = 10 })
                                , (var "b" { line = 1, column = 13 })
                                ]
                                { line = 1, column = 9 }
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        ]


record : Test
record =
    describe "Record"
        [ test "Simple record" <|
            \() ->
                "{a = b}"
                    |> isExpression
                        (Record
                            [ ( "a", (var "b" { line = 1, column = 5 }) )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simple record with many fields" <|
            \() ->
                "{a = b, b = 2}"
                    |> isExpression
                        (Record
                            [ ( "a", (var "b" { line = 1, column = 5 }) )
                            , ( "b", (Integer 2 { line = 1, column = 12 }) )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simple record with many tuple fields" <|
            \() ->
                "{a = (a, b), b = (a, b)}"
                    |> isExpression
                        (Record
                            [ ( "a"
                              , (Tuple
                                    [ (var "a" { line = 1, column = 6 })
                                    , (var "b" { line = 1, column = 9 })
                                    ]
                                    { line = 1, column = 5 }
                                )
                              )
                            , ( "b"
                              , (Tuple
                                    [ (var "a" { line = 1, column = 18 })
                                    , (var "b" { line = 1, column = 21 })
                                    ]
                                    { line = 1, column = 17 }
                                )
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simple record with updated field" <|
            \() ->
                "{a | b = 2, c = 3}"
                    |> isExpression
                        (RecordUpdate
                            "a"
                            [ ( "b", (Integer 2 { line = 1, column = 9 }) )
                            , ( "c", (Integer 3 { line = 1, column = 16 }) )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simple record with advanced field" <|
            \() ->
                "{a = Just 2}"
                    |> isExpression
                        (Record
                            [ ( "a"
                              , (Application
                                    (var "Just" { line = 1, column = 5 })
                                    (Integer 2 { line = 1, column = 10 })
                                    { line = 1, column = 9 }
                                )
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simple update record with advanced field" <|
            \() ->
                "{a | a = Just 2}"
                    |> isExpression
                        (RecordUpdate
                            "a"
                            [ ( "a"
                              , (Application
                                    (var "Just" { line = 1, column = 9 })
                                    (Integer 2 { line = 1, column = 14 })
                                    { line = 1, column = 13 }
                                )
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Simplified record destructuring pattern" <|
            \() ->
                "{a, b}"
                    |> isExpression
                        (Record
                            [ ( "a", var "a" { line = 1, column = 1 } )
                            , ( "b", var "b" { line = 1, column = 4 } )
                            ]
                            { line = 1, column = 0 }
                        )
        ]


expressions : Test
expressions =
    describe "Expressions"
        [ test "Operator in parens" <|
            \() -> "(+)" |> isExpression (var "+" { line = 1, column = 0 })
        , test "Operator passed to map" <|
            \() ->
                "reduce (+) list"
                    |> isExpression
                        (Application
                            (Application
                                (var "reduce" { line = 1, column = 0 })
                                (var "+" { line = 1, column = 7 })
                                { line = 1, column = 6 }
                            )
                            (var "list" { line = 1, column = 11 })
                            { line = 1, column = 10 }
                        )
        , test "partial application" <|
            \() ->
                "(+) 2"
                    |> isExpression
                        (Application
                            (var "+" { line = 1, column = 0 })
                            (Integer 2 { line = 1, column = 4 })
                            { line = 1, column = 3 }
                        )
        , test "Case with as" <|
            \() ->
                "case a of \nT _ as x -> 1"
                    |> isExpression
                        (Case
                            (var "a" { line = 1, column = 5 })
                            ([ ( BinOp
                                    (var "as" { line = 2, column = 4 })
                                    (Application
                                        (var "T" { line = 2, column = 0 })
                                        (var "_" { line = 2, column = 2 })
                                        { line = 2, column = 1 }
                                    )
                                    (var "x" { line = 2, column = 7 })
                                    { line = 2, column = 4 }
                               , Integer 1 { line = 2, column = 12 }
                               )
                             ]
                            )
                            { line = 1, column = 0 }
                        )
        , test "cons has right assoc" <|
            \() ->
                "a :: b :: c"
                    |> isExpression
                        (BinOp
                            (var "::" { line = 1, column = 4 })
                            (var "a" { line = 1, column = 0 })
                            (BinOp
                                (var "::" { line = 1, column = 9 })
                                (var "b" { line = 1, column = 5 })
                                (var "c" { line = 1, column = 10 })
                                { line = 1, column = 9 }
                            )
                            { line = 1, column = 4 }
                        )
        , test "cons has right assoc with tuple" <|
            \() ->
                "(a, a :: b :: c)"
                    |> isExpression
                        (Tuple
                            [ (var "a" { line = 1, column = 1 })
                            , ((BinOp
                                    (var "::" { line = 1, column = 8 })
                                    (var "a" { line = 1, column = 4 })
                               )
                                (BinOp
                                    (var "::" { line = 1, column = 13 })
                                    (var "b" { line = 1, column = 9 })
                                    (var "c" { line = 1, column = 14 })
                                    { line = 1, column = 13 }
                                )
                                { line = 1, column = 8 }
                              )
                            ]
                            { line = 1, column = 0 }
                        )
        , test "Destructuring lambda" <|
            \() ->
                "\\(a,b) acc -> 1"
                    |> isExpression
                        (Lambda
                            [ (Tuple
                                [ (var "a" { line = 1, column = 2 })
                                , (var "b" { line = 1, column = 4 })
                                ]
                                { line = 1, column = 1 }
                              )
                            , (var "acc" { line = 1, column = 7 })
                            ]
                            (Integer 1 { line = 1, column = 14 })
                            { line = 1, column = 0 }
                        )
        , test "Destructuring Let" <|
            \() ->
                "let (a,b) = (1,2) in a"
                    |> isExpression
                        (Let
                            [ ( (Tuple
                                    [ (var "a" { line = 1, column = 5 })
                                    , (var "b" { line = 1, column = 7 })
                                    ]
                                    { line = 1, column = 4 }
                                )
                              , (Tuple
                                    [ Integer 1
                                        { line = 1, column = 13 }
                                    , Integer 2 { line = 1, column = 15 }
                                    ]
                                    { line = 1, column = 12 }
                                )
                              )
                            ]
                            (var "a" { line = 1, column = 21 })
                            { line = 1, column = 0 }
                        )
        , test "Access" <|
            \() ->
                "Module.a"
                    |> isExpression
                        (Access
                            (var "Module" { line = 1, column = 0 })
                            [ "a" ]
                            { line = 1, column = 0 }
                        )
        , test "AccessFunction" <|
            \() ->
                "map .a list"
                    |> isExpression
                        (Application
                            (Application
                                (var "map" { line = 1, column = 0 })
                                (AccessFunction "a" { line = 1, column = 4 })
                                { line = 1, column = 3 }
                            )
                            (var "list" { line = 1, column = 7 })
                            { line = 1, column = 6 }
                        )
        ]
