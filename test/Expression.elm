module Expression where

import ElmTest exposing (..)

import Ast
import Ast.Expression exposing (..)
import Ast.Module exposing (opTable)
import Combine exposing (Context)

parseExpression : String -> (Result (List String) Expression, Context)
parseExpression = Ast.parseExpression opTable

is : String -> Expression -> Assertion
is s e =
  case parseExpression s of
    (Ok r, _) ->
      assertEqual r e

    _ ->
      assert False

fails : String -> Assertion
fails s =
  case parseExpression s of
    (Err _, _) ->
      assert True

    _ ->
      assert False

characterLiterals : Test
characterLiterals =
  suite "Character literals"
    [ test "character literal" ("'a'" `is` Character 'a')
    , test "newline literal" ("'\n'" `is` Character '\n')
    , test "character literals must contain one character" (fails "''")
    ]

intLiterals : Test
intLiterals =
  suite "Integer literals"
    [ test "integer literal" ("0" `is` Integer 0)
    , test "positive literal" ("+12" `is` Integer 12)
    , test "negative literal" ("-12" `is` Integer -12)
    ]

floatLiterals : Test
floatLiterals =
  suite "Float literals"
    [ test "float literal" ("0." `is` Float 0.0)
    , test "positive literal" ("+12.5" `is` Float 12.5)
    , test "negative literal" ("-12.5" `is` Float -12.5)
    ]

literals : Test
literals =
  suite "Literals"
    [ characterLiterals
    , intLiterals
    --, floatLiterals
    ]

letExpressions : Test
letExpressions =
  suite "Let"
    [ test "single binding"
        ("let a = 42 in a" `is` (Let
                                   [("a", Integer 42)]
                                   (Variable ["a"])))

    , test "multiple bindings"
        ("""
          let
            a = 42

            b = a + 1
          in
            b
         """ `is` (Let
                     [ ("a", Integer 42)
                     , ("b", (Application (Variable ["+"]) (Integer 1))) ]
                     (Variable ["b"])))
    ]

application : Test
application =
  suite "Application"
    [ test "simple application" ("f a" `is` (Application
                                               (Variable ["f"])
                                               (Variable ["a"])))
    , test "curried application" ("f a b" `is` (Application
                                                  (Application
                                                     (Variable ["f"])
                                                     (Variable ["a"]))
                                                  (Variable ["b"])))
    , test "constructor application" ("Cons a Nil" `is` (Application
                                                           (Application
                                                              (Variable ["Cons"])
                                                              (Variable ["a"]))
                                                           (Variable ["Nil"])))
    ]


all : Test
all =
  suite "Expression suite"
    [ literals
    , application
    ]
