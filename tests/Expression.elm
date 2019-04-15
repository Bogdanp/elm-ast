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

import Ast.Expression exposing (..)
import Helpers exposing (app, binOp, case_, fails, integer, isExpression, list, record, recordUpdate, tuple, var, wm)
import Test exposing (Test, describe, test)


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> isExpression (wm <| Character 'a')
        , test "newline literal" <|
            \() -> "'\n'" |> isExpression (wm <| Character '\n')
        , test "Charcode literals" <|
            \() -> "'\\x23'" |> isExpression (wm <| Character '#')
        , test "character literals must contain one character" <|
            \() -> fails "''"
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> isExpression (integer 0)
        , test "positive literal" <|
            \() -> "+12" |> isExpression (integer 12)
        , test "negative literal" <|
            \() -> "-12" |> isExpression (integer -12)
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> isExpression (wm <| Float 0.5)
        , test "positive literal" <|
            \() -> "+12.5" |> isExpression (wm <| Float 12.5)
        , test "negative literal" <|
            \() -> "-12.5" |> isExpression (wm <| Float -12.5)
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> isExpression (wm <| String "")
        , test "simple string" <|
            \() -> "\"hello\"" |> isExpression (wm <| String "hello")
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> isExpression (wm <| String "hello, \\\"world\\\"")
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> isExpression (wm <| String "")
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> isExpression (wm <| String "hello\nworld")
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> isExpression (wm <| String "\\\\")
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
                        (wm <|
                            Let
                                [ ( var "a", integer 42 ) ]
                                (var "a")
                        )
        , test "bind to _" <|
            \() ->
                "let _ = 42 in 24"
                    |> isExpression
                        (wm <|
                            Let
                                [ ( var "_", integer 42 ) ]
                                (integer 24)
                        )
        , test "Can start with a tag name" <|
            \() ->
                "let letter = 1 \n in letter"
                    |> isExpression
                        (wm <|
                            Let
                                [ ( var "letter", integer 1 ) ]
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
                    |> isExpression
                        (wm <|
                            Let
                                [ ( app (var "f") (var "x"), binOp (var "+") (var "x") (integer 1) ) ]
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
                    |> isExpression
                        (wm <|
                            Let
                                [ ( app (var "f") (var "x")
                                  , binOp (var "+") (var "x") (integer 1)
                                  )
                                , ( app (var "g") (var "x")
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
                    |> isExpression
                        (wm <|
                            Let
                                [ ( var "a", integer 42 )
                                , ( var "b", binOp (var "+") (var "a") (integer 1) )
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
                    |> isExpression
                        (case_
                            (var "x")
                            [ ( var "Nothing", integer 0 )
                            , ( app (var "Just") (var "y"), var "y" )
                            ]
                        )
        , test "binding to underscore" <|
            \() ->
                """
case x of
  _ ->
    42
          """
                    |> isExpression
                        (case_
                            (var "x")
                            [ ( var "_", integer 42 ) ]
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
                    |> isExpression
                        (case_
                            (var "x")
                            [ ( var "a", var "a" )
                            , ( var "b", case_ (var "y") [ ( var "a1", var "a1" ), ( var "b1", var "b1" ) ] )
                            , ( var "c", var "c" )
                            ]
                        )
        ]


application : Test
application =
    describe "app"
        [ test "simple application" <|
            \() ->
                "f a"
                    |> isExpression
                        (app
                            (var "f")
                            (var "a")
                        )
        , test "curried application" <|
            \() ->
                "f a b"
                    |> isExpression
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
                    |> isExpression
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
                    |> isExpression
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
                    |> isExpression
                        (app
                            (var "f")
                            (var "==")
                        )
        , test "same multiline bug" <|
            \() ->
                "f\n \"I like the symbol =\""
                    |> isExpression
                        (app
                            (var "f")
                            (wm <| String "I like the symbol =")
                        )
        , test "constructor application" <|
            \() ->
                "Cons a Nil"
                    |> isExpression
                        (app
                            (app
                                (var "Cons")
                                (var "a")
                            )
                            (var "Nil")
                        )
        , test "app with record update" <|
            \() ->
                "a  { r | f = 1 } c"
                    |> isExpression
                        (app
                            (app
                                (var "a")
                                (recordUpdate "r" [ ( wm "f", integer 1 ) ])
                            )
                            (var "c")
                        )
        ]


tuples : Test
tuples =
    describe "tuples"
        [ test "Empty tuple" <|
            \() -> "()" |> isExpression (var "()")
        , test "Simple tuple" <|
            \() ->
                "(a, b)"
                    |> isExpression
                        (tuple
                            [ var "a"
                            , var "b"
                            ]
                        )
        , test "Simple tuple with format" <|
            \() ->
                "( a, b )"
                    |> isExpression
                        (tuple
                            [ var "a"
                            , var "b"
                            ]
                        )
        ]


lists : Test
lists =
    describe "Lists"
        [ test "Empty list" <| \() -> "[]" |> isExpression (list [])
        , test "Simple list" <|
            \() -> "[1, 2]" |> isExpression (list [ integer 1, integer 2 ])
        , test "tuple list" <|
            \() ->
                "[(a, b), (a, b)]"
                    |> isExpression
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
                    |> isExpression
                        (record [ ( wm <| "a", var "b" ) ])
        , test "Simple record with many fields" <|
            \() ->
                "{a = b, b = 2}"
                    |> isExpression
                        (record
                            [ ( wm "a", var "b" )
                            , ( wm "b", integer 2 )
                            ]
                        )
        , test "Simple record with many tuple fields" <|
            \() ->
                "{a = (a, b), b = (a, b)}"
                    |> isExpression
                        (record
                            [ ( wm "a"
                              , tuple
                                    [ var "a"
                                    , var "b"
                                    ]
                              )
                            , ( wm "b"
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
                    |> isExpression
                        (recordUpdate
                            "a"
                            [ ( wm "b", integer 2 )
                            , ( wm "c", integer 3 )
                            ]
                        )
        , test "Simple record with advanced field" <|
            \() ->
                "{a = Just 2}"
                    |> isExpression
                        (record
                            [ ( wm "a"
                              , app
                                    (var "Just")
                                    (integer 2)
                              )
                            ]
                        )
        , test "Simple update record with advanced field" <|
            \() ->
                "{a | a = Just 2}"
                    |> isExpression
                        (recordUpdate
                            "a"
                            [ ( wm "a"
                              , app
                                    (var "Just")
                                    (integer 2)
                              )
                            ]
                        )
        , test "Simplified record destructuring pattern" <|
            \() ->
                "{a, b}"
                    |> isExpression
                        (record
                            [ ( wm "a"
                              , var "a"
                              )
                            , ( wm "b"
                              , var "b"
                              )
                            ]
                        )
        ]


expressions : Test
expressions =
    describe "Expressions"
        [ test "Operator in parens" <|
            \() -> "(+)" |> isExpression (var "+")
        , test "Operators passed to map" <|
            \() ->
                "reduce (+) list"
                    |> isExpression
                        (app
                            (app
                                (var "reduce")
                                (var "+")
                            )
                            (var "list")
                        )
        , test "partial application" <|
            \() -> "(+) 2" |> isExpression (app (var "+") (integer 2))
        , test "case_ with as" <|
            \() ->
                "case a of\n  T _ as x -> 1"
                    |> isExpression
                        (case_
                            (var "a")
                            [ ( binOp (var "as")
                                    (app (var "T")
                                        (var "_")
                                    )
                                    (var "x")
                              , integer 1
                              )
                            ]
                        )
        , test "cons has right assoc" <|
            \() ->
                "a :: b :: c"
                    |> isExpression
                        (binOp (var "::")
                            (var "a")
                            (binOp (var "::") (var "b") (var "c"))
                        )
        , test "cons has right assoc with tuple" <|
            \() ->
                "(a, a :: b :: c)"
                    |> isExpression
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
                    |> isExpression
                        (wm <| Lambda [ tuple [ var "a", var "b" ], var "acc" ] (integer 1))
        , test "Destructuring Let" <|
            \() ->
                "let (a,b) = (1,2) in a"
                    |> isExpression
                        (wm <|
                            Let
                                [ ( tuple [ var "a", var "b" ]
                                  , tuple [ integer 1, integer 2 ]
                                  )
                                ]
                                (var "a")
                        )
        , test "Access" <|
            \() ->
                "Module.a"
                    |> isExpression
                        (wm <| Access (var "Module") [ wm "a" ])
        , test "AccessFunction" <|
            \() ->
                "map .a list"
                    |> isExpression
                        (app (app (var "map") (wm <| AccessFunction "a")) (var "list"))
        ]
