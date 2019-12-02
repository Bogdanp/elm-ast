module Pattern exposing (aliases, characterLiterals, cons, constructor, floatLiterals, functions, intLiterals, stringLiterals, variable, wildcard)

import Helpers exposing (..)
import Test exposing (Test, describe, test)


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> isPatternSansMeta (characterPattern 'a')
        , test "newline literal" <|
            \() -> "'\n'" |> isPatternSansMeta (characterPattern '\n')
        , test "Charcode literals" <|
            \() -> "'\\x23'" |> isPatternSansMeta (characterPattern '#')
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> isPatternSansMeta (integerPattern 0)
        , test "positive literal" <|
            \() -> "12" |> isPatternSansMeta (integerPattern 12)
        , test "negative literal" <|
            \() -> "-12" |> isPatternSansMeta (integerPattern -12)
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> isPatternSansMeta (floatPattern 0.5)
        , test "positive literal" <|
            \() -> "12.5" |> isPatternSansMeta (floatPattern 12.5)
        , test "negative literal" <|
            \() -> "-12.5" |> isPatternSansMeta (floatPattern -12.5)
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> isPatternSansMeta (stringPattern "")
        , test "simple string" <|
            \() -> "\"hello\"" |> isPatternSansMeta (stringPattern "hello")
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> isPatternSansMeta (stringPattern "hello, \\\"world\\\"")
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> isPatternSansMeta (stringPattern "")
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> isPatternSansMeta (stringPattern "hello\nworld")
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> isPatternSansMeta (stringPattern "\\\\")
        ]


variable : Test
variable =
    describe "Variable"
        [ test "simple" <| \() -> "abc" |> isPatternSansMeta (variablePattern "abc")
        ]


constructor : Test
constructor =
    describe "Constructors"
        [ test "simple" <|
            \() ->
                "A b c"
                    |> isPatternSansMeta
                        (applicationPattern
                            (applicationPattern (constructorPattern "A") (variablePattern "b"))
                            (variablePattern "c")
                        )
        , test "literal" <|
            \() ->
                "A 1 2.5 'a' \"b\" _ c"
                    |> isPatternSansMeta
                        (applicationFromListSM (applicationPattern (constructorPattern "A") (integerPattern 1))
                            [ floatPattern 2.5
                            , characterPattern 'a'
                            , stringPattern "b"
                            , wildPattern
                            , variablePattern "c"
                            ]
                        )
        , test "recursive" <|
            \() ->
                "A (B 2) (C 3 4)"
                    |> isPatternSansMeta
                        (applicationPattern
                            (applicationPattern
                                (constructorPattern "A")
                                (applicationPattern (constructorPattern "B") (integerPattern 2))
                            )
                            (applicationPattern (applicationPattern (constructorPattern "C") (integerPattern 3)) (integerPattern 4))
                        )
        ]


aliases : Test
aliases =
    describe "Aliases"
        [ test "simple" <|
            \() ->
                "(1,2,3) as x"
                    |> isPatternSansMeta
                        (asPattern
                            (tuplePattern [ integerPattern 1, integerPattern 2, integerPattern 3 ])
                            "x"
                        )
        , test "nested" <|
            \() ->
                "[[1], [2, 3] as x] as y"
                    |> isPatternSansMeta
                        (asPattern
                            (listPattern
                                [ listPattern [ integerPattern 1 ]
                                , asPattern
                                    (listPattern
                                        [ integerPattern 2
                                        , integerPattern 3
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
                    |> isPatternSansMeta (asPattern (asPattern (variablePattern "x") "y") "z")
        , test "recursive 2" <|
            \() ->
                "(Nothing as x) as y"
                    |> isPatternSansMeta
                        (asPattern (asPattern (constructorPattern "Nothing") "x") "y")
        ]


cons : Test
cons =
    describe "Cons"
        [ test "multiple" <|
            \() ->
                "1 :: (((x))) :: []" |> isPatternSansMeta (consPattern (integerPattern 1) (consPattern (variablePattern "x") (listPattern [])))
        , test "mixed" <|
            \() ->
                "Nothing :: (x as y)" |> isPatternSansMeta (consPattern (constructorPattern "Nothing") (asPattern (variablePattern "x") "y"))
        ]


wildcard : Test
wildcard =
    describe "Wildcard"
        [ test "simple" <| \() -> "_" |> isPatternSansMeta wildPattern
        , test "constructor" <|
            \() ->
                "A _ _"
                    |> isPatternSansMeta
                        (applicationPattern (applicationPattern (constructorPattern "A") wildPattern) wildPattern)
        , test "list" <| \() -> "[x, _]" |> isPatternSansMeta (listPattern [ variablePattern "x", wildPattern ])
        , test "as" <|
            \() ->
                "[x, _] as y"
                    |> isPatternSansMeta (asPattern (listPattern [ variablePattern "x", wildPattern ]) "y")
        ]


functions : Test
functions =
    describe "Functions"
        [ test "simple" <|
            \() ->
                "f x y _"
                    |> isPatternSansMeta
                        (applicationFromListSM
                            (applicationPattern (variablePattern "f") (variablePattern "x"))
                            [ variablePattern "y"
                            , wildPattern
                            ]
                        )
        , test "with constructors" <|
            \() ->
                "a B c D e"
                    |> isPatternSansMeta
                        (applicationFromListSM (applicationPattern (variablePattern "a") (constructorPattern "B"))
                            [ variablePattern "c"
                            , constructorPattern "D"
                            , variablePattern "e"
                            ]
                        )
        , test "mixed" <|
            \() ->
                "f (Just x) Nothing y (a :: b as c) (_, 3.6)"
                    |> isPatternSansMeta
                        (applicationFromListSM (applicationPattern (variablePattern "f") (applicationPattern (constructorPattern "Just") (variablePattern "x")))
                            [ constructorPattern "Nothing"
                            , variablePattern "y"
                            , asPattern (consPattern (variablePattern "a") (variablePattern "b")) "c"
                            , tuplePattern [ wildPattern, floatPattern 3.6 ]
                            ]
                        )
        ]
