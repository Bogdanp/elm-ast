module Expression exposing (all)

import Test exposing (describe, test, Test)
import Expect exposing (..)

import Ast exposing (parseExpression)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)

type alias Expectation = Expect.Expectation

is : String -> Expression -> Expectation
is s e =
  case parseExpression operators s of
    (Ok r, _) ->
      Expect.equal e r

    _ ->
      Expect.fail ("not" ++ s)

fails : String -> Expectation
fails s =
  case parseExpression operators s of
    (Err _, _) ->
      Expect.pass

    _ ->
      Expect.fail (s ++ " expected to fail")

characterLiterals : Test
characterLiterals =
  describe "Character literals"
    [ test "character literal" <|
        \() -> "'a'" `is` Character 'a'

    , test "newline literal" <|
        \() -> "'\n'" `is` Character '\n'

    , test "character literals must contain one character" <|
        \() -> fails "''"
    ]

intLiterals : Test
intLiterals =
  describe "Integer literals"
    [ test "integer literal" <|
        \() -> "0" `is` Integer 0

    , test "positive literal" <|
        \() -> "+12" `is` Integer 12

    , test "negative literal" <|
        \() -> "-12" `is` Integer -12
    ]

floatLiterals : Test
floatLiterals =
  describe "Float literals"
    [ test "float literal" <|
        \() -> "0.5" `is` Float 0.5

    , test "positive literal" <|
        \() -> "+12.5" `is` Float 12.5

    , test "negative literal" <|
        \() -> "-12.5" `is` Float -12.5
    ]

stringLiterals : Test
stringLiterals =
  describe "String literals"
    [ test "empty string" <|
        \() -> "\"\"" `is` String ""

    , test "simple string" <|
        \() -> "\"hello\"" `is` String "hello"

    , test "escaped string" <|
        \() -> "\"hello, \\\"world\\\"\"" `is` String "hello, \\\"world\\\""

    , test "triple-quoted string" <|
        \() -> "\"\"\"\"\"\"" `is` String ""

    , test "multi-line strings" <|
        \() -> "\"\"\"hello\nworld\"\"\"" `is` String "hello\nworld"
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
        \() -> "let a = 42 in a" `is` (Let
                                         [("a", Integer 42)]
                                         (Variable ["a"]))

    , test "multiple bindings" <|
        \() -> """
            let
              a = 42

              b = a + 1
            in
              b
            """ `is` (Let
                        [ ("a", Integer 42)
                        , ("b", (Application (Variable ["+"]) (Integer 1)))
                        ]
                        (Variable ["b"]))
    ]

application : Test
application =
  describe "Application"
    [ test "simple application" <|
        \() -> "f a" `is` (Application
                              (Variable ["f"])
                              (Variable ["a"]))

    , test "curried application" <|
        \() -> "f a b" `is` (Application
                               (Application
                                  (Variable ["f"])
                                  (Variable ["a"]))
                               (Variable ["b"]))

    , test "constructor application" <|
        \() -> "Cons a Nil" `is` (Application
                                    (Application
                                       (Variable ["Cons"])
                                       (Variable ["a"]))
                                    (Variable ["Nil"]))
    ]


all : Test
all =
  describe "Expression suite"
    [ literals
    , application
    ]
