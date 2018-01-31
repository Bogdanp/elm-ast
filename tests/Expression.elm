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
import Test exposing (describe, test, Test)
import Helpers exposing (var, fails, isExpression)


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> isExpression (Character 'a')
        , test "newline literal" <|
            \() -> "'\n'" |> isExpression (Character '\n')
        , test "Charcode literals" <|
            \() -> "'\\x23'" |> isExpression (Character '#')
        , test "character literals must contain one character" <|
            \() -> fails "''"
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> isExpression (Integer 0)
        , test "positive literal" <|
            \() -> "+12" |> isExpression (Integer 12)
        , test "negative literal" <|
            \() -> "-12" |> isExpression (Integer -12)
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> isExpression (Float 0.5)
        , test "positive literal" <|
            \() -> "+12.5" |> isExpression (Float 12.5)
        , test "negative literal" <|
            \() -> "-12.5" |> isExpression (Float -12.5)
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> isExpression (String "")
        , test "simple string" <|
            \() -> "\"hello\"" |> isExpression (String "hello")
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> isExpression (String "hello, \\\"world\\\"")
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> isExpression (String "")
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> isExpression (String "hello\nworld")
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> isExpression (String "\\\\")
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
                            [ ( var "a", Integer 42 ) ]
                            (var "a")
                        )
        , test "bind to _" <|
            \() ->
                "let _ = 42 in 24"
                    |> isExpression
                        (Let
                            [ ( var "_", Integer 42 ) ]
                            (Integer 24)
                        )
        , test "Can start with a tag name" <|
            \() ->
                "let letter = 1 \n in letter"
                    |> isExpression
                        (Let
                            [ ( var "letter", Integer 1 ) ]
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
                        (Let
                            [ ( Application (var "f") (var "x"), (BinOp (var "+") (var "x") (Integer 1)) ) ]
                            (Application (var "f") (Integer 4))
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
                            [ ( (Application (var "f") (var "x"))
                              , (BinOp (var "+") (var "x") (Integer 1))
                              )
                            , ( Application (var "g") (var "x")
                              , (BinOp (var "+") (var "x") (Integer 1))
                              )
                            ]
                            (Application (var "f") (Integer 4))
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
                            [ ( var "a", Integer 42 )
                            , ( var "b", (BinOp (var "+") (var "a") (Integer 1)) )
                            ]
                            (var "b")
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
                            (var "x")
                            [ ( var "Nothing", Integer 0 )
                            , ( Application (var "Just") (var "y"), (var "y") )
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
                        (Case
                            (var "x")
                            [ ( var "_", Integer 42 ) ]
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
                        (Case
                            (var "x")
                            [ ( var "a", var "a" )
                            , ( var "b", Case (var "y") [ ( var "a1", var "a1" ), ( var "b1", var "b1" ) ] )
                            , ( var "c", var "c" )
                            ]
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
                            (var "f")
                            (var "a")
                        )
        , test "curried application" <|
            \() ->
                "f a b"
                    |> isExpression
                        (Application
                            (Application
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "curried application with parens" <|
            \() ->
                "(f a) b"
                    |> isExpression
                        (Application
                            (Application
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "multiline application" <|
            \() ->
                "  f\n   a\n b"
                    |> isExpression
                        (Application
                            (Application
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "multiline bug" <|
            \() ->
                "f\n (==)"
                    |> isExpression
                        (Application
                            (var "f")
                            (var "==")
                        )
        , test "same multiline bug" <|
            \() ->
                "f\n \"I like the symbol =\""
                    |> isExpression
                        (Application
                            (var "f")
                            (String "I like the symbol =")
                        )
        , test "constructor application" <|
            \() ->
                "Cons a Nil"
                    |> isExpression
                        (Application
                            (Application
                                (var "Cons")
                                (var "a")
                            )
                            (var "Nil")
                        )
        , test "Application with record update" <|
            \() ->
                "a  { r | f = 1 } c"
                    |> isExpression
                        (Application
                            (Application
                                (var "a")
                                (RecordUpdate "r" [ ( "f", Integer 1 ) ])
                            )
                            (var "c")
                        )
        ]


tuple : Test
tuple =
    describe "Tuples"
        [ test "Empty tuple" <|
            \() -> "()" |> isExpression (var "()")
        , test "Simple tuple" <|
            \() ->
                "(a, b)"
                    |> isExpression
                        (Tuple
                            [ (var "a")
                            , (var "b")
                            ]
                        )
        , test "Simple tuple with format" <|
            \() ->
                "( a, b )"
                    |> isExpression
                        (Tuple
                            [ (var "a")
                            , (var "b")
                            ]
                        )
        ]


list : Test
list =
    describe "Lists"
        [ test "Empty list" <| \() -> "[]" |> isExpression (List [])
        , test "Simple list" <|
            \() -> "[1, 2]" |> isExpression (List [ Integer 1, Integer 2 ])
        , test "Tuple list" <|
            \() ->
                "[(a, b), (a, b)]"
                    |> isExpression
                        (List
                            [ (Tuple
                                [ (var "a")
                                , (var "b")
                                ]
                              )
                            , (Tuple
                                [ (var "a")
                                , (var "b")
                                ]
                              )
                            ]
                        )
        ]


record : Test
record =
    describe "Record"
        [ test "Simple record" <|
            \() ->
                "{a = b}"
                    |> isExpression
                        (Record [ ( "a", (var "b") ) ])
        , test "Simple record with many fields" <|
            \() ->
                "{a = b, b = 2}"
                    |> isExpression
                        (Record
                            [ ( "a", (var "b") )
                            , ( "b", (Integer 2) )
                            ]
                        )
        , test "Simple record with many tuple fields" <|
            \() ->
                "{a = (a, b), b = (a, b)}"
                    |> isExpression
                        (Record
                            [ ( "a"
                              , (Tuple
                                    [ (var "a")
                                    , (var "b")
                                    ]
                                )
                              )
                            , ( "b"
                              , (Tuple
                                    [ (var "a")
                                    , (var "b")
                                    ]
                                )
                              )
                            ]
                        )
        , test "Simple record with updated field" <|
            \() ->
                "{a | b = 2, c = 3}"
                    |> isExpression
                        (RecordUpdate
                            "a"
                            [ ( "b", (Integer 2) )
                            , ( "c", (Integer 3) )
                            ]
                        )
        , test "Simple record with advanced field" <|
            \() ->
                "{a = Just 2}"
                    |> isExpression
                        (Record
                            [ ( "a"
                              , (Application
                                    (var "Just")
                                    (Integer 2)
                                )
                              )
                            ]
                        )
        , test "Simple update record with advanced field" <|
            \() ->
                "{a | a = Just 2}"
                    |> isExpression
                        (RecordUpdate
                            "a"
                            [ ( "a"
                              , (Application
                                    (var "Just")
                                    (Integer 2)
                                )
                              )
                            ]
                        )
        , test "Simplified record destructuring pattern" <|
            \() ->
                "{a, b}"
                    |> isExpression
                        (Record
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
            \() -> "(+)" |> isExpression (var "+")
        , test "Operators passed to map" <|
            \() ->
                "reduce (+) list"
                    |> isExpression
                        (Application
                            (Application
                                (var "reduce")
                                (var "+")
                            )
                            (var "list")
                        )
        , test "partial application" <|
            \() -> "(+) 2" |> isExpression (Application (var "+") (Integer 2))
        , test "Case with as" <|
            \() ->
                "case a of \nT _ as x -> 1"
                    |> isExpression
                        (Case
                            (var "a")
                            ([ ( BinOp (var "as")
                                    (Application (var "T")
                                        (var "_")
                                    )
                                    (var "x")
                               , Integer 1
                               )
                             ]
                            )
                        )
        , test "cons has right assoc" <|
            \() ->
                "a :: b :: c"
                    |> isExpression
                        (BinOp (var "::")
                            (var "a")
                            (BinOp (var "::") (var "b") (var "c"))
                        )
        , test "cons has right assoc with tuple" <|
            \() ->
                "(a, a :: b :: c)"
                    |> isExpression
                        (Tuple
                            [ (var "a")
                            , ((BinOp (var "::") (var "a"))
                                (BinOp (var "::") (var "b") (var "c"))
                              )
                            ]
                        )
        , test "Destructuring lambda" <|
            \() ->
                "\\(a,b) acc -> 1"
                    |> isExpression
                        (Lambda [ (Tuple [ (var "a"), (var "b") ]), (var "acc") ] (Integer 1))
        , test "Destructuring Let" <|
            \() ->
                "let (a,b) = (1,2) in a"
                    |> isExpression
                        (Let
                            [ ( (Tuple [ (var "a"), (var "b") ])
                              , (Tuple [ Integer 1, Integer 2 ])
                              )
                            ]
                            (var "a")
                        )
        , test "Access" <|
            \() ->
                "Module.a"
                    |> isExpression
                        (Access (var "Module") [ "a" ])
        , test "AccessFunction" <|
            \() ->
                "map .a list"
                    |> isExpression
                        (Application (Application (var "map") (AccessFunction "a")) (var "list"))
        ]
