module Statement
    exposing
        ( moduleDeclaration
        , importStatements
        , infixDeclarations
        , moduleFixityDeclarations
        , multipleDeclarations
        , portStatements
        , singleDeclaration
        , typeAnnotations
        , typeDeclarations
        )

import Ast.BinOp exposing (Assoc(..))
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Type(..), Statement(..))
import Test exposing (describe, test, Test)
import Helpers exposing (var, isStatement, areStatements)


moduleDeclaration : Test
moduleDeclaration =
    describe "Module declaration statements"
        [ test "simple declaration exposing all" <|
            \() ->
                "module A exposing (..)"
                    |> isStatement
                        (ModuleDeclaration [ "A" ] AllExport { line = 1, column = 0 })
        , test "declaration exposing particular things" <|
            \() ->
                "module A exposing (A, b)"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport
                                [ TypeExport "A" Nothing
                                , FunctionExport "b"
                                ]
                            )
                            { line = 1, column = 0 }
                        )
        , test "declaration exposing an infix operator" <|
            \() ->
                "module A exposing ((?))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport [ FunctionExport "?" ])
                            { line = 1, column = 0 }
                        )
        , test "declaration exposing union" <|
            \() ->
                "module A exposing (A(..))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport [ TypeExport "A" (Just AllExport) ])
                            { line = 1, column = 0 }
                        )
        , test "declaration exposing constructor subset" <|
            \() ->
                "module A exposing (A(A))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport
                                [ TypeExport "A"
                                    (Just <|
                                        SubsetExport [ FunctionExport "A" ]
                                    )
                                ]
                            )
                            { line = 1, column = 0 }
                        )
        , test "multiline declaration" <|
            \() ->
                "module A exposing (A, B,\nc)"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport
                                [ TypeExport "A" Nothing
                                , TypeExport "B" Nothing
                                , FunctionExport "c"
                                ]
                            )
                            { line = 1, column = 0 }
                        )
        , test "declaration using a port" <|
            \() ->
                "port module A exposing (A(..))"
                    |> isStatement
                        (PortModuleDeclaration [ "A" ]
                            (SubsetExport [ TypeExport "A" (Just AllExport) ])
                            { line = 1, column = 0 }
                        )
        , test "simple effects" <|
            \() ->
                "effect module A where {subscription = MySub, command = MyCmd} exposing (..)"
                    |> isStatement
                        (EffectModuleDeclaration [ "A" ]
                            [ ( "subscription", "MySub" )
                            , ( "command", "MyCmd" )
                            ]
                            AllExport
                            { line = 1, column = 0 }
                        )
        ]


importStatements : Test
importStatements =
    describe "Import statements"
        [ test "simple import" <|
            \() ->
                "import A"
                    |> isStatement
                        (ImportStatement
                            [ "A" ]
                            Nothing
                            Nothing
                            { line = 1, column = 0 }
                        )
        , test "import as" <|
            \() ->
                "import A as B"
                    |> isStatement
                        (ImportStatement
                            [ "A" ]
                            (Just "B")
                            Nothing
                            { line = 1, column = 0 }
                        )
        , test "import exposing all" <|
            \() ->
                "import A exposing (..)"
                    |> isStatement
                        (ImportStatement
                            [ "A" ]
                            Nothing
                            (Just AllExport)
                            { line = 1, column = 0 }
                        )
        , test "import exposing" <|
            \() ->
                "import A exposing (A, b)"
                    |> isStatement
                        (ImportStatement [ "A" ]
                            Nothing
                            (Just
                                (SubsetExport
                                    [ TypeExport "A" Nothing
                                    , FunctionExport "b"
                                    ]
                                )
                            )
                            { line = 1, column = 0 }
                        )
        , test "import exposing union" <|
            \() ->
                "import A exposing (A(..))"
                    |> isStatement
                        (ImportStatement [ "A" ]
                            Nothing
                            (Just
                                (SubsetExport
                                    [ TypeExport "A" (Just AllExport) ]
                                )
                            )
                            { line = 1, column = 0 }
                        )
        , test "import exposing constructor subset" <|
            \() ->
                "import A exposing (A(A))"
                    |> isStatement
                        (ImportStatement [ "A" ]
                            Nothing
                            (Just
                                (SubsetExport
                                    [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
                                )
                            )
                            { line = 1, column = 0 }
                        )
        , test "import multiline" <|
            \() ->
                "import A as B exposing (A, B,\nc)"
                    |> isStatement
                        (ImportStatement [ "A" ]
                            (Just "B")
                            (Just
                                (SubsetExport
                                    [ TypeExport "A" Nothing
                                    , TypeExport "B" Nothing
                                    , FunctionExport "c"
                                    ]
                                )
                            )
                            { line = 1, column = 0 }
                        )
        ]


typeAnnotations : Test
typeAnnotations =
    describe "Type annotations"
        [ test "constant" <|
            \() ->
                "x : Int"
                    |> isStatement
                        (FunctionTypeDeclaration
                            "x"
                            (TypeConstructor
                                [ "Int" ]
                                []
                                { line = 1, column = 4 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "variables" <|
            \() ->
                "x : a"
                    |> isStatement
                        (FunctionTypeDeclaration
                            "x"
                            (TypeVariable "a" { line = 1, column = 4 })
                            { line = 1, column = 0 }
                        )
        , test "variables with numbers" <|
            \() ->
                "x : a1"
                    |> isStatement
                        (FunctionTypeDeclaration
                            "x"
                            (TypeVariable "a1" { line = 1, column = 4 })
                            { line = 1, column = 0 }
                        )
        , test "application" <|
            \() ->
                "x : a -> b"
                    |> isStatement
                        (FunctionTypeDeclaration
                            "x"
                            (TypeApplication
                                (TypeVariable "a" { line = 1, column = 4 })
                                (TypeVariable "b" { line = 1, column = 9 })
                                { line = 1, column = 6 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "application associativity" <|
            \() ->
                "x : a -> b -> c"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
                            (TypeApplication
                                (TypeVariable "a" { line = 1, column = 4 })
                                (TypeApplication
                                    (TypeVariable "b" { line = 1, column = 9 })
                                    (TypeVariable "c" { line = 1, column = 14 })
                                    { line = 1, column = 11 }
                                )
                                { line = 1, column = 6 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "application parens" <|
            \() ->
                "x : (a -> b) -> c"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
                            (TypeApplication
                                (TypeApplication
                                    (TypeVariable "a" { line = 1, column = 5 })
                                    (TypeVariable "b" { line = 1, column = 10 })
                                    { line = 1, column = 7 }
                                )
                                (TypeVariable "c" { line = 1, column = 16 })
                                { line = 1, column = 13 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "qualified types" <|
            \() ->
                "m : Html.App Msg"
                    |> isStatement
                        (FunctionTypeDeclaration "m"
                            (TypeConstructor
                                [ "Html", "App" ]
                                [ (TypeConstructor [ "Msg" ] [] { line = 1, column = 13 }) ]
                                { line = 1, column = 4 }
                            )
                            { line = 1, column = 0 }
                        )
        ]


portStatements : Test
portStatements =
    describe "port type declaration"
        [ test "constant" <|
            \() ->
                "port focus : String -> Cmd msg"
                    |> isStatement
                        (PortTypeDeclaration
                            "focus"
                            (TypeApplication
                                (TypeConstructor
                                    [ "String" ]
                                    []
                                    { line = 1, column = 13 }
                                )
                                (TypeConstructor [ "Cmd" ]
                                    ([ TypeVariable
                                        "msg"
                                        { line = 1, column = 27 }
                                     ]
                                    )
                                    { line = 1, column = 23 }
                                )
                                { line = 1, column = 20 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "another port type declaration" <|
            \() ->
                "port users : (User -> msg) -> Sub msg"
                    |> isStatement
                        (PortTypeDeclaration
                            "users"
                            (TypeApplication
                                (TypeApplication
                                    (TypeConstructor [ "User" ] [] { line = 1, column = 14 })
                                    (TypeVariable
                                        "msg"
                                        { line = 1, column = 22 }
                                    )
                                    { line = 1, column = 19 }
                                )
                                (TypeConstructor [ "Sub" ]
                                    ([ TypeVariable "msg" { line = 1, column = 34 } ])
                                    { line = 1, column = 30 }
                                )
                                { line = 1, column = 27 }
                            )
                            { line = 1, column = 0 }
                        )
        , test "port definition" <|
            \() ->
                "port focus = Cmd.none"
                    |> isStatement
                        (PortDeclaration "focus"
                            []
                            (Access
                                (var "Cmd" { line = 1, column = 13 })
                                [ "none" ]
                                { line = 1, column = 13 }
                            )
                            { line = 1, column = 0 }
                        )
        ]


infixDeclarations : Test
infixDeclarations =
    describe "Infix declarations"
        [ test "non" <|
            \() ->
                "infix 9 :-"
                    |> isStatement (InfixDeclaration N 9 ":-" { line = 1, column = 0 })
        , test "left" <|
            \() ->
                "infixl 9 :-"
                    |> isStatement (InfixDeclaration L 9 ":-" { line = 1, column = 0 })
        , test "right" <|
            \() ->
                "infixr 9 :-"
                    |> isStatement (InfixDeclaration R 9 ":-" { line = 1, column = 0 })
        ]


singleDeclarationInput : String
singleDeclarationInput =
    """
f : Int -> Int
f x =
  x + 1
"""


singleDeclaration : Test
singleDeclaration =
    test "simple function" <|
        \() ->
            singleDeclarationInput
                |> areStatements
                    [ FunctionTypeDeclaration "f"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [] { line = 1, column = 3 })
                            (TypeConstructor [ "Int" ] [] { line = 1, column = 10 })
                            { line = 1, column = 7 }
                        )
                        { line = 1, column = 0 }
                    , FunctionDeclaration "f"
                        ([ var "x" { line = 2, column = 1 } ])
                        (BinOp
                            (var "+" { line = 3, column = 4 })
                            (var "x" { line = 3, column = 1 })
                            (Integer 1 { line = 3, column = 5 })
                            { line = 3, column = 4 }
                        )
                        { line = 2, column = 0 }
                    ]


multipleDeclarationsInput : String
multipleDeclarationsInput =
    """


f : Int -> Int
f x =
  x + 1

g : Int -> Int
g x =
  f x + 1

h : (Int, Int) -> Int
h (a, b) = a + b

(+) : Int -> Int
(+) a b =
  1
"""


multipleDeclarations : Test
multipleDeclarations =
    test "multiple functions" <|
        \() ->
            multipleDeclarationsInput
                |> areStatements
                    [ FunctionTypeDeclaration "f"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [] { line = 3, column = 3 })
                            (TypeConstructor [ "Int" ] [] { line = 3, column = 10 })
                            { line = 3, column = 7 }
                        )
                        { line = 3, column = 0 }
                    , FunctionDeclaration "f"
                        ([ var "x" { line = 4, column = 1 } ])
                        (BinOp
                            (var "+" { line = 5, column = 4 })
                            (var "x" { line = 5, column = 1 })
                            (Integer 1 { line = 5, column = 5 })
                            { line = 5, column = 4 }
                        )
                        { line = 4, column = 0 }
                    , FunctionTypeDeclaration "g"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [] { line = 7, column = 3 })
                            (TypeConstructor [ "Int" ] [] { line = 7, column = 10 })
                            { line = 7, column = 7 }
                        )
                        { line = 7, column = 0 }
                    , FunctionDeclaration "g"
                        ([ var "x" { line = 8, column = 1 } ])
                        (BinOp
                            (var "+" { line = 9, column = 6 })
                            (Application
                                (var "f" { line = 9, column = 1 })
                                (var "x" { line = 9, column = 3 })
                                { line = 9, column = 2 }
                            )
                            (Integer 1 { line = 9, column = 7 })
                            { line = 9, column = 6 }
                        )
                        { line = 8, column = 0 }
                    , FunctionTypeDeclaration "h"
                        (TypeApplication
                            (TypeTuple
                                ([ TypeConstructor [ "Int" ]
                                    []
                                    { line = 11, column = 4 }
                                 , TypeConstructor
                                    [ "Int" ]
                                    []
                                    { line = 11, column = 9 }
                                 ]
                                )
                                { line = 11, column = 3 }
                            )
                            (TypeConstructor
                                [ "Int" ]
                                []
                                { line = 11, column = 17 }
                            )
                            { line = 11, column = 14 }
                        )
                        { line = 11, column = 0 }
                    , FunctionDeclaration "h"
                        ([ Tuple
                            ([ var "a" { line = 12, column = 2 }
                             , var "b" { line = 12, column = 5 }
                             ]
                            )
                            { line = 12, column = 1 }
                         ]
                        )
                        (BinOp
                            (var "+" { line = 12, column = 13 })
                            (var "a" { line = 12, column = 10 })
                            (var "b" { line = 12, column = 14 })
                            { line = 12, column = 13 }
                        )
                        { line = 12, column = 0 }
                    , FunctionTypeDeclaration "+"
                        (TypeApplication
                            (TypeConstructor
                                [ "Int" ]
                                []
                                { line = 14, column = 5 }
                            )
                            (TypeConstructor
                                [ "Int" ]
                                []
                                { line = 14, column = 12 }
                            )
                            { line = 14, column = 9 }
                        )
                        { line = 14, column = 0 }
                    , FunctionDeclaration "+"
                        ([ var "a" { line = 15, column = 3 }
                         , var "b" { line = 15, column = 5 }
                         ]
                        )
                        (Integer 1 { line = 16, column = 1 })
                        { line = 15, column = 0 }
                    ]


moduleFixityInput : String
moduleFixityInput =
    """
f = a ++ b ++ c

infixl 1 ++

g = a ** b ** c

infixr 1 **
"""


moduleFixityDeclarations : Test
moduleFixityDeclarations =
    test "module fixity scanning" <|
        \() ->
            moduleFixityInput
                |> areStatements
                    [ FunctionDeclaration "f"
                        []
                        (BinOp
                            (var "++" { line = 1, column = 12 })
                            (BinOp
                                (var "++" { line = 1, column = 7 })
                                (var "a" { line = 1, column = 3 })
                                (var "b" { line = 1, column = 8 })
                                { line = 1, column = 7 }
                            )
                            (var "c" { line = 1, column = 13 })
                            { line = 1, column = 12 }
                        )
                        { line = 1, column = 0 }
                    , InfixDeclaration L 1 "++" { line = 3, column = 0 }
                    , FunctionDeclaration "g"
                        []
                        (BinOp
                            (var "**" { line = 5, column = 7 })
                            (var "a" { line = 5, column = 3 })
                            (BinOp
                                (var "**" { line = 5, column = 12 })
                                (var "b" { line = 5, column = 8 })
                                (var "c" { line = 5, column = 13 })
                                { line = 5, column = 12 }
                            )
                            { line = 5, column = 7 }
                        )
                        { line = 5, column = 0 }
                    , InfixDeclaration R 1 "**" { line = 7, column = 0 }
                    ]


emptyRecordAliasInput : String
emptyRecordAliasInput =
    """
type alias A = {}
"""


emptyTupleAliasInput : String
emptyTupleAliasInput =
    """
type alias A = ()
"""


typeDeclarations : Test
typeDeclarations =
    describe "type declarations"
        [ test "can parse empty record aliases" <|
            \() ->
                emptyRecordAliasInput
                    |> areStatements
                        [ TypeAliasDeclaration
                            (TypeConstructor [ "A" ] [] { line = 1, column = 10 })
                            (TypeRecord [] { line = 1, column = 15 })
                            { line = 1, column = 0 }
                        ]
        , test "can parse aliases of unit" <|
            \() ->
                emptyTupleAliasInput
                    |> areStatements
                        [ TypeAliasDeclaration
                            (TypeConstructor [ "A" ] [] { line = 1, column = 10 })
                            (TypeTuple [] { line = 1, column = 14 })
                            { line = 1, column = 0 }
                        ]
        ]
