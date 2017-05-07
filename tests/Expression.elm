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
    (Ok (_, _, r)) ->
      Expect.equal e r

    (Err (_, { position }, es)) ->
      Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)

fails : String -> Expectation
fails s =
  case parseExpression operators s of
    (Err _) ->
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
        \() -> "let a = 42 in a" |> is ((Let
                                         [("a", [], Integer 42)]
                                         (Variable ["a"])))

    , test "bind to _" <|
        \() -> "let _ = 42 in 24" |> is ((Let
                                          [("_", [], Integer 42)]
                                          (Integer 24)))

    , test "function" <|
        \() -> """
let
  f x = x + 1
in
  f 4
        """ |> is ((Let
                    [("f", ["x"], (BinOp (Variable ["+"]) (Variable ["x"]) (Integer 1)))]
                    (Application (Variable ["f"]) (Integer 4))))
   , test "function" <|
        \() -> """
let
  f x = x + 1
  g x = x + 1
in
  f 4
        """ |> is ((Let
                    [("f", ["x"], (BinOp (Variable ["+"]) (Variable ["x"]) (Integer 1)))
                    ,("g", ["x"], (BinOp (Variable ["+"]) (Variable ["x"]) (Integer 1)))]
                    (Application (Variable ["f"]) (Integer 4))))

    , test "multiple bindings" <|
        \() -> """
let
  a = 42

  b = a + 1
in
  b
            """ |> is ((Let
                        [ ("a", [], Integer 42)
                        , ("b", [], (BinOp (Variable ["+"]) (Variable ["a"]) (Integer 1)))
                        ]
                        (Variable ["b"])))
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
          """ |> is ((Case
                      (Variable ["x"])
                      [ (Variable ["Nothing"], Integer 0)
                      , (Application (Variable ["Just"]) (Variable ["y"]), (Variable ["y"]))
                      ]))

    , test "binding to underscore" <|
        \() ->
          """
case x of
  _ ->
    42
          """ |> is ((Case
                     (Variable ["x"])
                     [(Variable ["_"], Integer 42)]))
    ]

application : Test
application =
  describe "Application"
    [ test "simple application" <|
        \() -> "f a" |> is ((Application
                              (Variable ["f"])
                              (Variable ["a"])))

    , test "curried application" <|
        \() -> "f a b" |> is ((Application
                               (Application
                                  (Variable ["f"])
                                  (Variable ["a"]))
                               (Variable ["b"])))

    , test "curried application with parens" <|
        \() -> "(f a) b" |> is ((Application
                                   (Application
                                        (Variable ["f"])
                                        (Variable ["a"]))
                                   (Variable ["b"])))

    , test "constructor application" <|
        \() -> "Cons a Nil" |> is ((Application
                                    (Application
                                       (Variable ["Cons"])
                                       (Variable ["a"]))
                                    (Variable ["Nil"])))
    ]

tuple : Test
tuple =
    describe "Tuples"
        [ test "Empty tuple" <|
              \() -> "()" |> is (Variable ["()"])
         ,test "Simple tuple" <|
              \() -> "(a, b)" |> is (BinOp
                                          (Variable [","])
                                          (Variable ["a"])
                                          (Variable ["b"])
                                     )
        , test "Simple tuple with format" <|
            \() -> "( a, b )" |> is (BinOp
                                          (Variable [","])
                                          (Variable ["a"])
                                          (Variable ["b"])
                                     )
        ]

record : Test
record =
    describe "Tuples"
        [ test "Simple record" <|
              \() -> "{a = b}" |> is (Record [
                                           ("a" , (Variable ["b"]))
                                           ]
                                     )
        , test "Simple record with many fields" <|
              \() -> "{a = b, b = 2}" |> is (Record
                                                 [("a" , (Variable ["b"]))
                                                 , ("b" , (Integer 2))
                                           ]
                                     )
        , test "Simple record with updated field" <|
            \() -> "{a | b = 2, c = 3}" |> is (RecordUpdate
                                                   "a"
                                                   [ ("b" , (Integer 2))
                                                   , ("c" , (Integer 3))
                                                   ]
                                              )


        ]

expressions : Test
expressions =
    describe "Expressions"
        [ test "Operator in parens" <|
            \() -> "(+)" |> is (Variable ["+"])
        , test "Operators passed to map" <|
            \() -> "reduce (+) list" |> is (Application
                                             (Application
                                                  (Variable ["reduce"])
                                                  (Variable ["+"]))
                                             (Variable ["list"]))
        , test "partial application" <|
            \() -> "(+) 2" |> is (Application (Variable ["+"]) (Integer 2))

        , test "Case with as" <|
            \() -> "case a of \nT _ as x -> 1" |> is (
                                                      Case
                                                          (Variable ["a"])
                                                          ([(BinOp (Variable ["as"])
                                                                 (Application (Variable ["T"])
                                                                      (Variable ["_"])) (Variable ["x"])
                                                            , Integer 1)])
                                                )
        ]

all : Test
all =
  describe "Expression suite"
    [ literals
    , letExpressions
    , caseExpressions
    , application
    , tuple
    , record
    , expressions
    ]
