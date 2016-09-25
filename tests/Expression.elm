module Expression exposing (..)

import Test exposing (..)

import Ast exposing (parseExpression)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Combine exposing (Context)

is : String -> Expression -> Assertion
is s e =
  case parseExpression operators s of
    (Ok r, _) ->
      assertEqual e r

    _ ->
      assert False

fails : String -> Assertion
fails s =
  case parseExpression operators s of
    (Err _, _) ->
      assert True

    _ ->
      assert False

characterLiterals : Test
characterLiterals =
  suite "Character literals"
    [ test "character literal"
        <| "'a'" `is` Character 'a'

    , test "newline literal"
        <| "'\n'" `is` Character '\n'

    , test "character literals must contain one character"
        <| fails "''"
    ]

intLiterals : Test
intLiterals =
  suite "Integer literals"
    [ test "integer literal"
        <| "0" `is` Integer 0

    , test "positive literal"
        <| "+12" `is` Integer 12

    , test "negative literal"
        <| "-12" `is` Integer -12
    ]

floatLiterals : Test
floatLiterals =
  suite "Float literals"
    [ test "float literal"
        <| "0.5" `is` Float 0.5

    , test "positive literal"
        <| "+12.5" `is` Float 12.5

    , test "negative literal"
        <| "-12.5" `is` Float -12.5
    ]

stringLiterals : Test
stringLiterals =
  suite "String literals"
    [ test "empty string"
        <| "\"\"" `is` String ""

    , test "simple string"
        <| "\"hello\"" `is` String "hello"

    , test "escaped string"
        <| "\"hello, \\\"world\\\"\"" `is` String "hello, \\\"world\\\""

    , test "triple-quoted string"
        <| "\"\"\"\"\"\"" `is` String ""

    , test "multi-line strings"
        <| "\"\"\"hello\nworld\"\"\"" `is` String "hello\nworld"
    ]

literals : Test
literals =
  suite "Literals"
    [ characterLiterals
    , intLiterals
    , floatLiterals
    , stringLiterals
    ]

letExpressions : Test
letExpressions =
  suite "Let"
    [ test "single binding"
        <| "let a = 42 in a" `is` (Let
                                     [("a", Integer 42)]
                                     (Variable ["a"]))

    , test "multiple bindings"
        <| """
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
  suite "Application"
    [ test "simple application"
        <| "f a" `is` (Application
                         (Variable ["f"])
                         (Variable ["a"]))

    , test "curried application"
        <| "f a b" `is` (Application
                           (Application
                              (Variable ["f"])
                              (Variable ["a"]))
                           (Variable ["b"]))

    , test "constructor application"
        <| "Cons a Nil" `is` (Application
                                (Application
                                   (Variable ["Cons"])
                                   (Variable ["a"]))
                                (Variable ["Nil"]))
    ]


all : Test
all =
  suite "Expression suite"
    [ literals
    , application
    ]
