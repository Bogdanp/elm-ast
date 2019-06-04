module Statement exposing
    ( importStatements
    , infixDeclarations
    , moduleDeclarations
    , moduleFixityDeclarations
    , multipleDeclarations
    , portStatements
    , singleDeclaration
    , typeAnnotations
    , typeDeclarations
    )

import Ast.BinOp exposing (Assoc(..))
import Ast.Statement exposing (..)
import Helpers exposing (..)
import Test exposing (Test, describe, test)


moduleDeclarations : Test
moduleDeclarations =
    describe "Module declaration statements"
        [ test "simple declaration exposing all" <|
            \() -> "module A exposing (..)" |> isStatementSansMeta (moduleDeclaration [ "A" ] AllExport)
        , test "declaration exposing particular things" <|
            \() ->
                "module A exposing (A, b)"
                    |> isStatementSansMeta
                        (moduleDeclaration [ "A" ] <|
                            SubsetExport
                                [ TypeExport "A" Nothing
                                , FunctionExport "b"
                                ]
                        )
        , test "declaration exposing an infix operator" <|
            \() ->
                "module A exposing ((?))"
                    |> isStatementSansMeta
                        (moduleDeclaration [ "A" ] <|
                            SubsetExport [ FunctionExport "?" ]
                        )
        , test "declaration exposing union" <|
            \() ->
                "module A exposing (A(..))"
                    |> isStatementSansMeta
                        (moduleDeclaration [ "A" ] <|
                            SubsetExport [ TypeExport "A" (Just AllExport) ]
                        )
        , test "declaration exposing constructor subset" <|
            \() ->
                "module A exposing (A(A))"
                    |> isStatementSansMeta
                        (moduleDeclaration [ "A" ] <|
                            SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
                        )
        , test "multiline declaration" <|
            \() ->
                "module A exposing (A, B,\nc)"
                    |> isStatementSansMeta
                        (moduleDeclaration [ "A" ] <|
                            SubsetExport
                                [ TypeExport "A" Nothing
                                , TypeExport "B" Nothing
                                , FunctionExport "c"
                                ]
                        )
        , test "declaration using a port" <|
            \() ->
                "port module A exposing (A(..))"
                    |> isStatementSansMeta
                        (portModuleDeclaration [ "A" ] <|
                            SubsetExport [ TypeExport "A" (Just AllExport) ]
                        )
        , test "simple effects" <|
            \() ->
                "effect module A where {subscription = MySub, command = MyCmd} exposing (..)"
                    |> isStatementSansMeta
                        (effectModuleDeclaration [ "A" ]
                            [ ( "subscription", "MySub" )
                            , ( "command", "MyCmd" )
                            ]
                            AllExport
                        )
        ]


importStatements : Test
importStatements =
    describe "Import statements"
        [ test "simple import" <|
            \() -> "import A" |> isStatementSansMeta (importStatement [ "A" ] Nothing Nothing)
        , test "import as" <|
            \() -> "import A as B" |> isStatementSansMeta (importStatement [ "A" ] (Just "B") Nothing)
        , test "import exposing all" <|
            \() ->
                "import A exposing (..)"
                    |> isStatementSansMeta (importStatement [ "A" ] Nothing (Just AllExport))
        , test "import exposing" <|
            \() ->
                "import A exposing (A, b)"
                    |> isStatementSansMeta
                        (importStatement [ "A" ] Nothing <|
                            Just <|
                                SubsetExport
                                    [ TypeExport "A" Nothing
                                    , FunctionExport "b"
                                    ]
                        )
        , test "import exposing union" <|
            \() ->
                "import A exposing (A(..))"
                    |> isStatementSansMeta
                        (importStatement [ "A" ] Nothing <|
                            Just <|
                                SubsetExport [ TypeExport "A" (Just AllExport) ]
                        )
        , test "import exposing constructor subset" <|
            \() ->
                "import A exposing (A(A))"
                    |> isStatementSansMeta
                        (importStatement [ "A" ] Nothing <|
                            Just <|
                                SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
                        )
        , test "import multiline" <|
            \() ->
                "import A as B exposing (A, B,\nc)"
                    |> isStatementSansMeta
                        (importStatement [ "A" ] (Just "B") <|
                            Just <|
                                SubsetExport
                                    [ TypeExport "A" Nothing
                                    , TypeExport "B" Nothing
                                    , FunctionExport "c"
                                    ]
                        )
        ]


typeAnnotations : Test
typeAnnotations =
    describe "Type annotations"
        [ test "constant" <|
            \() ->
                "x : Int"
                    |> isStatementSansMeta (functionTypeDeclaration "x" (TypeConstructor [ "Int" ] []))
        , test "variables" <|
            \() ->
                "x : a"
                    |> isStatementSansMeta (functionTypeDeclaration "x" (TypeVariable "a"))
        , test "variables with numbers" <|
            \() ->
                "x : a1"
                    |> isStatementSansMeta (functionTypeDeclaration "x" (TypeVariable "a1"))
        , test "application" <|
            \() ->
                "x : a -> b"
                    |> isStatementSansMeta
                        (functionTypeDeclaration "x"
                            (TypeApplication
                                (TypeVariable "a")
                                (TypeVariable "b")
                            )
                        )
        , test "application associativity" <|
            \() ->
                "x : a -> b -> c"
                    |> isStatementSansMeta
                        (functionTypeDeclaration "x"
                            (TypeApplication
                                (TypeVariable "a")
                                (TypeApplication
                                    (TypeVariable "b")
                                    (TypeVariable "c")
                                )
                            )
                        )
        , test "application parens" <|
            \() ->
                "x : (a -> b) -> c"
                    |> isStatementSansMeta
                        (functionTypeDeclaration "x"
                            (TypeApplication
                                (TypeApplication
                                    (TypeVariable "a")
                                    (TypeVariable "b")
                                )
                                (TypeVariable "c")
                            )
                        )
        , test "qualified types" <|
            \() ->
                "m : Html.App Msg"
                    |> isStatementSansMeta
                        (functionTypeDeclaration "m"
                            (TypeConstructor
                                [ "Html", "App" ]
                                [ TypeConstructor [ "Msg" ] [] ]
                            )
                        )
        ]


portStatements : Test
portStatements =
    describe "port type declaration"
        [ test "constant" <|
            \() ->
                "port focus : String -> Cmd msg"
                    |> isStatementSansMeta
                        (portTypeDeclaration "focus"
                            (TypeApplication
                                (TypeConstructor [ "String" ] [])
                                (TypeConstructor [ "Cmd" ]
                                    [ TypeVariable "msg" ]
                                )
                            )
                        )
        , test "another port type declaration" <|
            \() ->
                "port users : (User -> msg) -> Sub msg"
                    |> isStatementSansMeta
                        (portTypeDeclaration "users"
                            (TypeApplication
                                (TypeApplication
                                    (TypeConstructor [ "User" ] [])
                                    (TypeVariable "msg")
                                )
                                (TypeConstructor [ "Sub" ]
                                    [ TypeVariable "msg" ]
                                )
                            )
                        )
        , test "port definition" <|
            \() ->
                "port focus = Cmd.none"
                    |> isStatementSansMeta
                        (portDeclaration "focus"
                            []
                            (ExternalSM
                                [ "Cmd" ]
                                (var "none")
                            )
                        )
        ]


infixDeclarations : Test
infixDeclarations =
    describe "Infix declarations"
        [ test "non" <|
            \() ->
                "infix 9 :-"
                    |> isStatementSansMeta (infixDeclaration N 9 ":-")
        , test "left" <|
            \() ->
                "infixl 9 :-"
                    |> isStatementSansMeta (infixDeclaration L 9 ":-")
        , test "right" <|
            \() ->
                "infixr 9 :-"
                    |> isStatementSansMeta (infixDeclaration R 9 ":-")
        ]


singleDeclarationInput : String
singleDeclarationInput =
    """
f : Int -> Int
f x =
  a { r | f = 1 }    c
"""


singleDeclaration : Test
singleDeclaration =
    test "simple function" <|
        \() ->
            singleDeclarationInput
                |> areStatementsSansMeta
                    [ functionTypeDeclaration "f"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , functionDeclaration
                        (applicationPattern (variablePattern "f")
                            (variablePattern "x")
                        )
                        (app
                            (app
                                (var "a")
                                (recordUpdate "r" [ ( "f", integer 1 ) ])
                            )
                            (var "c")
                        )
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
                |> areStatementsSansMeta
                    [ functionTypeDeclaration "f"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , functionDeclaration
                        (applicationPattern (variablePattern "f")
                            (variablePattern "x")
                        )
                        (binOp
                            (var "+")
                            (var "x")
                            (integer 1)
                        )
                    , functionTypeDeclaration "g"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , functionDeclaration
                        (applicationPattern (variablePattern "g")
                            (variablePattern "x")
                        )
                        (binOp
                            (var "+")
                            (app
                                (var "f")
                                (var "x")
                            )
                            (integer 1)
                        )
                    , functionTypeDeclaration "h"
                        (TypeApplication
                            (TypeTuple [ TypeConstructor [ "Int" ] [], TypeConstructor [ "Int" ] [] ])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , functionDeclaration
                        (applicationPattern (variablePattern "h")
                            (tuplePattern [ variablePattern "a", variablePattern "b" ])
                        )
                        (binOp
                            (var "+")
                            (var "a")
                            (var "b")
                        )
                    , functionTypeDeclaration "+"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , functionDeclaration
                        (applicationPattern
                            (applicationPattern (variablePattern "+") (variablePattern "a"))
                            (variablePattern "b")
                        )
                        (integer 1)
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
                |> areStatementsSansMeta
                    [ functionDeclaration
                        (variablePattern "f")
                        (binOp
                            (var "++")
                            (binOp (var "++") (var "a") (var "b"))
                            (var "c")
                        )
                    , infixDeclaration L 1 "++"
                    , functionDeclaration
                        (variablePattern "g")
                        (binOp
                            (var "**")
                            (var "a")
                            (binOp (var "**") (var "b") (var "c"))
                        )
                    , infixDeclaration R 1 "**"
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
                emptyRecordAliasInput |> areStatementsSansMeta [ typeAliasDeclaration (TypeConstructor [ "A" ] []) (TypeRecord []) ]
        , test "can parse aliases of unit" <|
            \() ->
                emptyTupleAliasInput |> areStatementsSansMeta [ typeAliasDeclaration (TypeConstructor [ "A" ] []) (TypeTuple []) ]
        ]
