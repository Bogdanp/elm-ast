module Expression exposing (all)

import Ast exposing (parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Expect exposing (..)
import String
import Test exposing (describe, test, Test)


is : Expression -> String -> Expectation
is e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e r

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


fails : String -> Expectation
fails s =
    case parseExpression operators s of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail (s ++ " expected to fail")


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> is (Character 'a')
        , test "newline literal" <|
            \() -> "'\n'" |> is (Character '\n')
        , test "character literals must contain one character" <|
            \() -> fails "''"
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> is (Integer 0)
        , test "positive literal" <|
            \() -> "+12" |> is (Integer 12)
        , test "negative literal" <|
            \() -> "-12" |> is (Integer -12)
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> is (Float 0.5)
        , test "positive literal" <|
            \() -> "+12.5" |> is (Float 12.5)
        , test "negative literal" <|
            \() -> "-12.5" |> is (Float -12.5)
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> is (String "")
        , test "simple string" <|
            \() -> "\"hello\"" |> is (String "hello")
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> is (String "hello, \\\"world\\\"")
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> is (String "")
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> is (String "hello\nworld")
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> is (String "\\\\")
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
                    |> is
                        (Let
                            [ ( var "a", Integer 42 ) ]
                            (var "a")
                        )
        , test "bind to _" <|
            \() ->
                "let _ = 42 in 24"
                    |> is
                        (Let
                            [ ( var "_", Integer 42 ) ]
                            (Integer 24)
                        )
        , test "function" <|
            \() ->
                """
let
  f x = x + 1
in
  f 4
        """
                    |> is
                        (Let
                            [ ( Application (var "f") (var "x"), (BinOp (var "+") (var "x") (Integer 1)) ) ]
                            (Application (var "f") (Integer 4))
                        )
        , test "function" <|
            \() ->
                """
let
  f x = x + 1
  g x = x + 1
in
  f 4
        """
                    |> is
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
                    |> is
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
                    |> is
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
                    |> is
                        (Case
                            (var "x")
                            [ ( var "_", Integer 42 ) ]
                        )
        ]


application : Test
application =
    describe "Application"
        [ test "simple application" <|
            \() ->
                "f a"
                    |> is
                        (Application
                            (var "f")
                            (var "a")
                        )
        , test "curried application" <|
            \() ->
                "f a b"
                    |> is
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
                    |> is
                        (Application
                            (Application
                                (var "f")
                                (var "a")
                            )
                            (var "b")
                        )
        , test "multiline application" <|
            \() ->
                "f\n a\n b"
                    |> is
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
                    |> is
                        (Application
                            (var "f")
                            (var "==")
                        )
        , test "same multiline bug" <|
            \() ->
                "f\n \"I like the symbol =\""
                    |> is
                        (Application
                            (var "f")
                            (String "I like the symbol =")
                        )
        , test "constructor application" <|
            \() ->
                "Cons a Nil"
                    |> is
                        (Application
                            (Application
                                (var "Cons")
                                (var "a")
                            )
                            (var "Nil")
                        )
        ]


tuple : Test
tuple =
    describe "Tuples"
        [ test "Empty tuple" <|
            \() -> "()" |> is (var "()")
        , test "Simple tuple" <|
            \() ->
                "(a, b)"
                    |> is
                        (Tuple
                            [ (var "a")
                            , (var "b")
                            ]
                        )
        , test "Simple tuple with format" <|
            \() ->
                "( a, b )"
                    |> is
                        (Tuple
                            [ (var "a")
                            , (var "b")
                            ]
                        )
        ]


list : Test
list =
    describe "Lists"
        [ test "Empty list" <| \() -> "[]" |> is (List [])
        , test "Simple list" <|
            \() -> "[1, 2]" |> is (List [ Integer 1, Integer 2 ])
        , test "Simple list" <|
            \() ->
                "[(a, b), (a, b)]"
                    |> is
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
    describe "Tuples"
        [ test "Simple record" <|
            \() ->
                "{a = b}"
                    |> is
                        (Record [ ( "a", (var "b") ) ])
        , test "Simple record with many fields" <|
            \() ->
                "{a = b, b = 2}"
                    |> is
                        (Record
                            [ ( "a", (var "b") )
                            , ( "b", (Integer 2) )
                            ]
                        )
        , test "Simple record with many tuple fields" <|
            \() ->
                "{a = (a, b), b = (a, b)}"
                    |> is
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
                    |> is
                        (RecordUpdate
                            "a"
                            [ ( "b", (Integer 2) )
                            , ( "c", (Integer 3) )
                            ]
                        )
        , test "Simple record with advanced field" <|
            \() ->
                "{a = Just 2}"
                    |> is
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
                    |> is
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
        ]


expressions : Test
expressions =
    describe "Expressions"
        [ test "Operator in parens" <|
            \() -> "(+)" |> is (var "+")
        , test "Operators passed to map" <|
            \() ->
                "reduce (+) list"
                    |> is
                        (Application
                            (Application
                                (var "reduce")
                                (var "+")
                            )
                            (var "list")
                        )
        , test "partial application" <|
            \() -> "(+) 2" |> is (Application (var "+") (Integer 2))
        , test "Case with as" <|
            \() ->
                "case a of \nT _ as x -> 1"
                    |> is
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
                    |> is
                        (BinOp (var "::")
                            (var "a")
                            (BinOp (var "::") (var "b") (var "c"))
                        )
        , test "cons has right assoc with tuple" <|
            \() ->
                "(a, a :: b :: c)"
                    |> is
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
                    |> is
                        (Lambda [ (Tuple [ (var "a"), (var "b") ]), (var "acc") ] (Integer 1))
        , test "Destructuring Let" <|
            \() ->
                "let (a,b) = (1,2) in a"
                    |> is
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
                    |> is
                        (Access (var "Module") [ "a" ])
        , test "AccessFunction" <|
            \() ->
                "map .a list"
                    |> is
                        (Application (Application (var "map") (AccessFunction "a")) (var "list"))
        ]


var a =
    Variable [ a ]


all : Test
all =
    describe "Expression suite"
        [ literals
        , letExpressions
        , caseExpressions
        , application
        , tuple
        , list
        , record
        , expressions
        ]
