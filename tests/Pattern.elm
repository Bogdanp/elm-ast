module Pattern exposing (aliases, characterLiterals, cons, constructor, floatLiterals, functions, intLiterals, stringLiterals, variable, wildcard)

import Ast.Common exposing (Literal(..), Pattern(..))
import Ast.Pattern exposing (..)
import Helpers exposing (..)
import Test exposing (Test, describe, test)


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> isPattern (characterPattern 'a')
        , test "newline literal" <|
            \() -> "'\n'" |> isPattern (characterPattern '\n')
        , test "Charcode literals" <|
            \() -> "'\\x23'" |> isPattern (characterPattern '#')
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> isPattern (integerPattern 0)
        , test "positive literal" <|
            \() -> "+12" |> isPattern (integerPattern 12)
        , test "negative literal" <|
            \() -> "-12" |> isPattern (integerPattern -12)
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> isPattern (floatPattern 0.5)
        , test "positive literal" <|
            \() -> "+12.5" |> isPattern (floatPattern 12.5)
        , test "negative literal" <|
            \() -> "-12.5" |> isPattern (floatPattern -12.5)
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> isPattern (stringPattern "")
        , test "simple string" <|
            \() -> "\"hello\"" |> isPattern (stringPattern "hello")
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> isPattern (stringPattern "hello, \\\"world\\\"")
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> isPattern (stringPattern "")
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> isPattern (stringPattern "hello\nworld")
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> isPattern (stringPattern "\\\\")
        ]


variable : Test
variable =
    describe "Variable"
        [ test "simple" <| \() -> "abc" |> isPattern (PVariable "abc")
        ]


constructor : Test
constructor =
    describe "Constructors"
        [ test "simple" <|
            \() ->
                "A b c"
                    |> isPattern
                        (PApplication
                            (PApplication (PConstructor "A") (PVariable "b"))
                            (PVariable "c")
                        )
        , test "literal" <|
            \() ->
                "A 1 2.5 'a' \"b\" _ c"
                    |> isPattern
                        (applicationFromList (PApplication (PConstructor "A") (integerPattern 1))
                            [ floatPattern 2.5
                            , characterPattern 'a'
                            , stringPattern "b"
                            , PWild
                            , PVariable "c"
                            ]
                        )
        , test "recursive" <|
            \() ->
                "A (B 2) (C 3 4)"
                    |> isPattern
                        (PApplication
                            (PApplication
                                (PConstructor "A")
                                (PApplication (PConstructor "B") (integerPattern 2))
                            )
                            (PApplication (PApplication (PConstructor "C") (integerPattern 3)) (integerPattern 4))
                        )
        ]


aliases : Test
aliases =
    describe "Aliases"
        [ test "simple" <|
            \() ->
                "(1,2,3) as x" |> isPattern (PAs (PTuple [ integerPattern 1, integerPattern 2, integerPattern 3 ]) "x")
        , test "nested" <|
            \() ->
                "[[1], [2, 3] as x] as y"
                    |> isPattern
                        (PAs
                            (PList
                                [ PList [ PLiteral (Integer 1) ]
                                , PAs
                                    (PList
                                        [ PLiteral (Integer 2)
                                        , PLiteral (Integer 3)
                                        ]
                                    )
                                    "x"
                                ]
                            )
                            "y"
                        )
        , test "recursive" <|
            \() ->
                "x as y as z"
                    |> isPattern (PAs (PAs (PVariable "x") "y") "z")
        , test "recursive 2" <|
            \() ->
                "(Nothing as x) as y"
                    |> isPattern
                        (PAs (PAs (PConstructor "Nothing") "x") "y")
        ]


cons : Test
cons =
    describe "Cons"
        [ test "multiple" <|
            \() ->
                "1 :: (((x))) :: []" |> isPattern (PCons (PLiteral (Integer 1)) (PCons (PVariable "x") (PList [])))
        , test "mixed" <|
            \() ->
                "Nothing :: (x as y)" |> isPattern (PCons (PConstructor "Nothing") (PAs (PVariable "x") "y"))
        ]


wildcard : Test
wildcard =
    describe "Wildcard"
        [ test "simple" <| \() -> "_" |> isPattern PWild
        , test "constructor" <|
            \() ->
                "A _ _"
                    |> isPattern
                        (PApplication (PApplication (PConstructor "A") PWild) PWild)
        , test "list" <| \() -> "[x, _]" |> isPattern (PList [ PVariable "x", PWild ])
        , test "as" <|
            \() ->
                "[x, _] as y"
                    |> isPattern (PAs (PList [ PVariable "x", PWild ]) "y")
        ]


functions : Test
functions =
    describe "Functions"
        [ test "simple" <|
            \() ->
                "f x y _"
                    |> isPattern
                        (applicationFromList
                            (PApplication (PVariable "f") (PVariable "x"))
                            [ PVariable "y"
                            , PWild
                            ]
                        )
        , test "with constructors" <|
            \() ->
                "a B c D e"
                    |> isPattern
                        (applicationFromList (PApplication (PVariable "a") (PConstructor "B"))
                            [ PVariable "c"
                            , PConstructor "D"
                            , PVariable "e"
                            ]
                        )
        , test "mixed" <|
            \() ->
                "f (Just x) Nothing y (a :: b as c) (_, 3.6)"
                    |> isPattern
                        (applicationFromList (PApplication (PVariable "f") (PApplication (PConstructor "Just") (PVariable "x")))
                            [ PConstructor "Nothing"
                            , PVariable "y"
                            , PAs (PCons (PVariable "a") (PVariable "b")) "c"
                            , PTuple [ PWild, PLiteral <| Float 3.6 ]
                            ]
                        )
        ]
