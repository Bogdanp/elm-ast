module Expression exposing
    ( application
    , caseExpressions
    , characterLiterals
    , expressions
    , floatLiterals
    , intLiterals
    , letExpressions
    , lists
    , literals
    , records
    , stringLiterals
    , tuples
    )

import Helpers exposing (..)
import Test exposing (Test, describe, test)


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> isExpressionSansMeta (character 'a')
        , test "newline literal" <|
            \() -> "'\n'" |> isExpressionSansMeta (character '\n')
        , test "Charcode literals" <|
            \() -> "'\\x23'" |> isExpressionSansMeta (character '#')
        , test "character literals must contain one character" <|
            \() -> fails "''"
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> isExpressionSansMeta (integer 0)
        , test "positive literal" <|
            \() -> "+12" |> isExpressionSansMeta (integer 12)
        , test "negative literal" <|
            \() -> "-12" |> isExpressionSansMeta (integer -12)
        , test "hex literal" <|
            \() -> "0x00002Fa" |> isExpressionSansMeta (integer 762)
        , test "negative hex literal" <|
            \() -> "-0x01cA" |> isExpressionSansMeta (integer -458)
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> isExpressionSansMeta (float 0.5)
        , test "positive literal" <|
            \() -> "+12.5" |> isExpressionSansMeta (float 12.5)
        , test "negative literal" <|
            \() -> "-12.5" |> isExpressionSansMeta (float -12.5)
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> isExpressionSansMeta (string "")
        , test "simple string" <|
            \() -> "\"hello\"" |> isExpressionSansMeta (string "hello")
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> isExpressionSansMeta (string "hello, \\\"world\\\"")
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> isExpressionSansMeta (string "")
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> isExpressionSansMeta (string "hello\nworld")
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> isExpressionSansMeta (string "\\\\")
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
                    |> isExpressionSansMeta
                        (let_
                            [ ( variablePattern "a", integer 42 ) ]
                            (var "a")
                        )
        , test "bind to _" <|
            \() ->
                "let _ = 42 in 24"
                    |> isExpressionSansMeta
                        (let_
                            [ ( wildPattern, integer 42 ) ]
                            (integer 24)
                        )
        , test "Can start with a tag name" <|
            \() ->
                "let letter = 1 \n in letter"
                    |> isExpressionSansMeta
                        (let_
                            [ ( variablePattern "letter", integer 1 ) ]
                            (var "letter")
                        )
        , test "function 1" <|
            \() ->
                """
let
  f x = x + 1
in
  f 4
        """
                    |> isExpressionSansMeta
                        (let_
                            [ ( applicationPattern (variablePattern "f") (variablePattern "x"), binOp (var "+") (var "x") (integer 1) ) ]
                            (app (var "f") (integer 4))
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
                    |> isExpressionSansMeta
                        (let_
                            [ ( applicationPattern (variablePattern "f") (variablePattern "x")
                              , binOp (var "+") (var "x") (integer 1)
                              )
                            , ( applicationPattern (variablePattern "g") (variablePattern "x")
                              , binOp (var "+") (var "x") (integer 1)
                              )
                            ]
                            (app (var "f") (integer 4))
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
                    |> isExpressionSansMeta
                        (let_
                            [ ( variablePattern "a", integer 42 )
                            , ( variablePattern "b", binOp (var "+") (var "a") (integer 1) )
                            ]
                            (var "b")
                        )
        ]


caseExpressions : Test
caseExpressions =
    describe "case_"
        [ test "simple statement" <|
            \() ->
                """
case x of
  Nothing ->
    0

  Just y ->
    y
          """
                    |> isExpressionSansMeta
                        (case_
                            (var "x")
                            [ ( constructorPattern "Nothing", integer 0 )
                            , ( applicationPattern (constructorPattern "Just") (variablePattern "y"), var "y" )
                            ]
                        )
        , test "binding to underscore" <|
            \() ->
                """
case x of
  _ ->
    42
          """
                    |> isExpressionSansMeta
                        (case_
                            (var "x")
                            [ ( wildPattern, integer 42 ) ]
                        )
        , test "Nested case" <|
            \() ->
                """
case x of
  a -> a
  b ->
    case y of
      a1 -> a1
      b1 -> b1
  c -> c
            """
                    |> isExpressionSansMeta
                        (case_
                            (var "x")
                            [ ( variablePattern "a", var "a" )
                            , ( variablePattern "b"
                              , case_ (var "y")
                                    [ ( variablePattern "a1", var "a1" )
                                    , ( variablePattern "b1", var "b1" )
                                    ]
                              )
                            , ( variablePattern "c", var "c" )
                            ]
                        )
        ]


application : Test
application =
    describe "app"
        [ test "simple application" <|
            \() ->
                "f a"
                    |> isExpressionSansMeta
                        (app
                            (var "f")
                            (var "a")
                        )
        , test "curried application" <|
            \() ->
                "f a b"
                    |> isExpressionSansMeta
                        (app
                            (app
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "curried application with parens" <|
            \() ->
                "(f a) b"
                    |> isExpressionSansMeta
                        (app
                            (app
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "multiline application" <|
            \() ->
                "  f\n   a\n b"
                    |> isExpressionSansMeta
                        (app
                            (app
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "multiline bug" <|
            \() ->
                "f\n (==)"
                    |> isExpressionSansMeta
                        (app
                            (var "f")
                            (var "==")
                        )
        , test "same multiline bug" <|
            \() ->
                "f\n \"I like the symbol =\""
                    |> isExpressionSansMeta
                        (app
                            (var "f")
                            (string "I like the symbol =")
                        )
        , test "constructor application" <|
            \() ->
                "Cons a Nil"
                    |> isExpressionSansMeta
                        (app
                            (app
                                (ConstructorSM "Cons")
                                (var "a")
                            )
                            (ConstructorSM "Nil")
                        )
        , test "app with record update" <|
            \() ->
                "a  { r | f = 1 } c"
                    |> isExpressionSansMeta
                        (app
                            (app
                                (var "a")
                                (recordUpdate "r" [ ( "f", integer 1 ) ])
                            )
                            (var "c")
                        )
        ]


tuples : Test
tuples =
    describe "tuples"
        [ test "Empty tuple" <|
            \() -> "()" |> isExpressionSansMeta (var "()")
        , test "Simple tuple" <|
            \() ->
                "(a, b)"
                    |> isExpressionSansMeta
                        (tuple
                            [ var "a"
                            , var "b"
                            ]
                        )
        , test "Simple tuple with format" <|
            \() ->
                "( a, b )"
                    |> isExpressionSansMeta
                        (tuple
                            [ var "a"
                            , var "b"
                            ]
                        )
        ]


lists : Test
lists =
    describe "Lists"
        [ test "Empty list" <| \() -> "[]" |> isExpressionSansMeta (list [])
        , test "Simple list" <|
            \() -> "[1, 2]" |> isExpressionSansMeta (list [ integer 1, integer 2 ])
        , test "tuple list" <|
            \() ->
                "[(a, b), (a, b)]"
                    |> isExpressionSansMeta
                        (list
                            [ tuple
                                [ var "a"
                                , var "b"
                                ]
                            , tuple
                                [ var "a"
                                , var "b"
                                ]
                            ]
                        )
        ]


records : Test
records =
    describe "Record"
        [ test "Simple record" <|
            \() ->
                "{a = b}"
                    |> isExpressionSansMeta
                        (record [ ( "a", var "b" ) ])
        , test "Simple record with many fields" <|
            \() ->
                "{a = b, b = 2}"
                    |> isExpressionSansMeta
                        (record
                            [ ( "a", var "b" )
                            , ( "b", integer 2 )
                            ]
                        )
        , test "Simple record with many tuple fields" <|
            \() ->
                "{a = (a, b), b = (a, b)}"
                    |> isExpressionSansMeta
                        (record
                            [ ( "a"
                              , tuple
                                    [ var "a"
                                    , var "b"
                                    ]
                              )
                            , ( "b"
                              , tuple
                                    [ var "a"
                                    , var "b"
                                    ]
                              )
                            ]
                        )
        , test "Simple record with updated field" <|
            \() ->
                "{a | b = 2, c = 3}"
                    |> isExpressionSansMeta
                        (recordUpdate
                            "a"
                            [ ( "b", integer 2 )
                            , ( "c", integer 3 )
                            ]
                        )
        , test "Simple record with advanced field" <|
            \() ->
                "{a = Just 2}"
                    |> isExpressionSansMeta
                        (record
                            [ ( "a"
                              , app
                                    (ConstructorSM "Just")
                                    (integer 2)
                              )
                            ]
                        )
        , test "Simple update record with advanced field" <|
            \() ->
                "{a | a = Just 2}"
                    |> isExpressionSansMeta
                        (recordUpdate
                            "a"
                            [ ( "a"
                              , app
                                    (ConstructorSM "Just")
                                    (integer 2)
                              )
                            ]
                        )
        , test "Simplified record destructuring pattern" <|
            \() ->
                "{a, b}"
                    |> isExpressionSansMeta
                        (record
                            [ ( "a"
                              , var "a"
                              )
                            , ( "b"
                              , var "b"
                              )
                            ]
                        )
        ]


expressions : Test
expressions =
    describe "Expressions"
        [ test "Operator in parens" <|
            \() -> "(+)" |> isExpressionSansMeta (var "+")
        , test "Operators passed to map" <|
            \() ->
                "reduce (+) list"
                    |> isExpressionSansMeta
                        (app
                            (app
                                (var "reduce")
                                (var "+")
                            )
                            (var "list")
                        )
        , test "partial application" <|
            \() -> "(+) 2" |> isExpressionSansMeta (app (var "+") (integer 2))
        , test "case_ with as" <|
            \() ->
                "case a of\n  T _ as x -> 1"
                    |> isExpressionSansMeta
                        (case_
                            (var "a")
                            [ ( asPattern (applicationPattern (constructorPattern "T") wildPattern) "x"
                              , integer 1
                              )
                            ]
                        )
        , test "cons has right assoc" <|
            \() ->
                "a :: b :: c"
                    |> isExpressionSansMeta
                        (binOp (var "::")
                            (var "a")
                            (binOp (var "::") (var "b") (var "c"))
                        )
        , test "cons has right assoc with tuple" <|
            \() ->
                "(a, a :: b :: c)"
                    |> isExpressionSansMeta
                        (tuple
                            [ var "a"
                            , binOp (var "::")
                                (var "a")
                                (binOp (var "::") (var "b") (var "c"))
                            ]
                        )
        , test "Destructuring lambda" <|
            \() ->
                "\\(a,b) acc -> 1"
                    |> isExpressionSansMeta
                        (lambda [ tuplePattern [ variablePattern "a", variablePattern "b" ], variablePattern "acc" ] (integer 1))
        , test "Destructuring Let" <|
            \() ->
                "let (a,b) = (1,2) in a"
                    |> isExpressionSansMeta
                        (let_
                            [ ( tuplePattern [ variablePattern "a", variablePattern "b" ]
                              , tuple [ integer 1, integer 2 ]
                              )
                            ]
                            (var "a")
                        )
        , test "Access" <|
            \() ->
                "Module.a"
                    |> isExpressionSansMeta
                        (ExternalSM [ "Module" ] (var "a"))
        , test "AccessFunction" <|
            \() ->
                "map .a list"
                    |> isExpressionSansMeta
                        (app (app (var "map") (accessFun "a")) (var "list"))
        ]
