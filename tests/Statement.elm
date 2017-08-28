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


meta =
    { line = 0, column = 0 }


moduleDeclaration : Test
moduleDeclaration =
    describe "Module declaration statements"
        [ test "simple declaration exposing all" <|
            \() -> "module A exposing (..)" |> isStatement (ModuleDeclaration [ "A" ] AllExport meta)
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
                            meta
                        )
        , test "declaration exposing an infix operator" <|
            \() ->
                "module A exposing ((?))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport [ FunctionExport "?" ])
                            meta
                        )
        , test "declaration exposing union" <|
            \() ->
                "module A exposing (A(..))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport [ TypeExport "A" (Just AllExport) ])
                            meta
                        )
        , test "declaration exposing constructor subset" <|
            \() ->
                "module A exposing (A(A))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ]
                            (SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ])
                            meta
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
                            meta
                        )
        , test "declaration using a port" <|
            \() ->
                "port module A exposing (A(..))"
                    |> isStatement
                        (PortModuleDeclaration [ "A" ]
                            (SubsetExport [ TypeExport "A" (Just AllExport) ])
                            meta
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
                            meta
                        )
        ]


importStatements : Test
importStatements =
    describe "Import statements"
        [ test "simple import" <|
            \() -> "import A" |> isStatement (ImportStatement [ "A" ] Nothing Nothing meta)
        , test "import as" <|
            \() -> "import A as B" |> isStatement (ImportStatement [ "A" ] (Just "B") Nothing meta)
        , test "import exposing all" <|
            \() ->
                "import A exposing (..)"
                    |> isStatement (ImportStatement [ "A" ] Nothing (Just AllExport) meta)
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
                            meta
                        )
        , test "import exposing union" <|
            \() ->
                "import A exposing (A(..))"
                    |> isStatement
                        (ImportStatement [ "A" ]
                            Nothing
                            (Just
                                (SubsetExport [ TypeExport "A" (Just AllExport) ])
                            )
                            meta
                        )
        , test "import exposing constructor subset" <|
            \() ->
                "import A exposing (A(A))"
                    |> isStatement
                        (ImportStatement [ "A" ]
                            Nothing
                            (Just
                                (SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ])
                            )
                            meta
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
                            meta
                        )
        ]


typeAnnotations : Test
typeAnnotations =
    describe "Type annotations"
        [ test "constant" <|
            \() ->
                "x : Int"
                    |> isStatement
                        (FunctionTypeDeclaration "x" (TypeConstructor [ "Int" ] [] meta) meta)
        , test "variables" <|
            \() ->
                "x : a"
                    |> isStatement
                        (FunctionTypeDeclaration "x" (TypeVariable "a" meta) meta)
        , test "variables with numbers" <|
            \() ->
                "x : a1"
                    |> isStatement
                        (FunctionTypeDeclaration "x" (TypeVariable "a1" meta) meta)
        , test "application" <|
            \() ->
                "x : a -> b"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
                            (TypeApplication
                                (TypeVariable "a" meta)
                                (TypeVariable "b" meta)
                            )
                            meta
                        )
        , test "application associativity" <|
            \() ->
                "x : a -> b -> c"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
                            (TypeApplication
                                (TypeVariable "a" meta)
                                (TypeApplication
                                    (TypeVariable "b" meta)
                                    (TypeVariable "c" meta)
                                )
                            )
                            meta
                        )
        , test "application parens" <|
            \() ->
                "x : (a -> b) -> c"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
                            (TypeApplication
                                (TypeApplication
                                    (TypeVariable "a" meta)
                                    (TypeVariable "b" meta)
                                )
                                (TypeVariable "c" meta)
                            )
                            meta
                        )
        , test "qualified types" <|
            \() ->
                "m : Html.App Msg"
                    |> isStatement
                        (FunctionTypeDeclaration "m"
                            (TypeConstructor
                                [ "Html", "App" ]
                                [ (TypeConstructor [ "Msg" ] []) meta ]
                                meta
                            )
                            meta
                        )
        ]


portStatements : Test
portStatements =
    describe "port type declaration"
        [ test "constant" <|
            \() ->
                "port focus : String -> Cmd msg"
                    |> isStatement
                        (PortTypeDeclaration "focus"
                            (TypeApplication
                                (TypeConstructor [ "String" ] [] meta)
                                (TypeConstructor [ "Cmd" ]
                                    [ TypeVariable "msg" meta ]
                                    meta
                                )
                            )
                            meta
                        )
        , test "another port type declaration" <|
            \() ->
                "port users : (User -> msg) -> Sub msg"
                    |> isStatement
                        (PortTypeDeclaration "users"
                            (TypeApplication
                                (TypeApplication
                                    (TypeConstructor [ "User" ] [] meta)
                                    (TypeVariable "msg" meta)
                                )
                                (TypeConstructor [ "Sub" ]
                                    [ TypeVariable "msg" meta ]
                                    meta
                                )
                            )
                            meta
                        )
        , test "port definition" <|
            \() ->
                "port focus = Cmd.none"
                    |> isStatement
                        (PortDeclaration "focus"
                            []
                            (Access
                                (Variable [ "Cmd" ])
                                [ "none" ]
                                meta
                            )
                            meta
                        )
        ]


infixDeclarations : Test
infixDeclarations =
    describe "Infix declarations"
        [ test "non" <|
            \() ->
                "infix 9 :-"
                    |> isStatement (InfixDeclaration N 9 ":-" meta)
        , test "left" <|
            \() ->
                "infixl 9 :-"
                    |> isStatement (InfixDeclaration L 9 ":-" meta)
        , test "right" <|
            \() ->
                "infixr 9 :-"
                    |> isStatement (InfixDeclaration R 9 ":-" meta)
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
                            (TypeConstructor [ "Int" ] [] meta)
                            (TypeConstructor [ "Int" ] [] meta)
                        )
                        meta
                    , FunctionDeclaration "f"
                        [ Variable [ "x" ] ]
                        (BinOp
                            (Variable [ "+" ])
                            (Variable [ "x" ])
                            (Integer 1 meta)
                        )
                        meta
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
                            (TypeConstructor [ "Int" ] [] meta)
                            (TypeConstructor [ "Int" ] [] meta)
                        )
                        meta
                    , FunctionDeclaration "f"
                        [ Variable [ "x" ] ]
                        (BinOp
                            (Variable [ "+" ])
                            (Variable [ "x" ])
                            (Integer 1 meta)
                        )
                        meta
                    , FunctionTypeDeclaration "g"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [] meta)
                            (TypeConstructor [ "Int" ] [] meta)
                        )
                        meta
                    , FunctionDeclaration "g"
                        [ Variable [ "x" ] ]
                        (BinOp
                            (Variable [ "+" ])
                            (Application
                                (Variable [ "f" ])
                                (Variable [ "x" ])
                            )
                            (Integer 1 meta)
                        )
                        meta
                    , FunctionTypeDeclaration "h"
                        (TypeApplication
                            (TypeTuple
                                [ TypeConstructor [ "Int" ] [] meta
                                , TypeConstructor [ "Int" ] [] meta
                                ]
                                meta
                            )
                            (TypeConstructor [ "Int" ] [] meta)
                        )
                        meta
                    , FunctionDeclaration "h"
                        [ Tuple [ (Variable [ "a" ]), (Variable [ "b" ]) ] meta ]
                        (BinOp
                            (Variable [ "+" ])
                            (Variable [ "a" ])
                            (Variable [ "b" ])
                        )
                        meta
                    , FunctionTypeDeclaration "+"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [] meta)
                            (TypeConstructor [ "Int" ] [] meta)
                        )
                        meta
                    , FunctionDeclaration "+" [ Variable [ "a" ], Variable [ "b" ] ] (Integer 1 meta) meta
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
                            (Variable [ "++" ])
                            (BinOp
                                (Variable [ "++" ])
                                (Variable [ "a" ])
                                (Variable [ "b" ])
                            )
                            (Variable [ "c" ])
                        )
                        meta
                    , InfixDeclaration L 1 "++" meta
                    , FunctionDeclaration "g"
                        []
                        (BinOp
                            (Variable [ "**" ])
                            (Variable [ "a" ])
                            (BinOp
                                (Variable [ "**" ])
                                (Variable [ "b" ])
                                (Variable [ "c" ])
                            )
                        )
                        meta
                    , InfixDeclaration R 1 "**" meta
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
                            (TypeConstructor [ "A" ] [] meta)
                            (TypeRecord [] meta)
                            meta
                        ]
        , test "can parse aliases of unit" <|
            \() ->
                emptyTupleAliasInput
                    |> areStatements
                        [ TypeAliasDeclaration
                            (TypeConstructor [ "A" ] [] meta)
                            (TypeTuple [] meta)
                            meta
                        ]
        ]
