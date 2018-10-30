module Statement exposing
    ( importStatements
    , infixDeclarations
    , moduleDeclaration
    , moduleFixityDeclarations
    , multipleDeclarations
    , portStatements
    , singleDeclaration
    , typeAnnotations
    , typeDeclarations
    )

import Ast.BinOp exposing (Assoc(..))
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Statement(..), Type(..))
import Helpers exposing (areStatements, isStatement, var)
import Test exposing (Test, describe, test)


moduleDeclaration : Test
moduleDeclaration =
    describe "Module declaration statements"
        [ test "simple declaration exposing all" <|
            \() -> "module A exposing (..)" |> isStatement (ModuleDeclaration [ "A" ] AllExport)
        , test "declaration exposing particular things" <|
            \() ->
                "module A exposing (A, b)"
                    |> isStatement
                        (ModuleDeclaration [ "A" ] <|
                            SubsetExport
                                [ TypeExport "A" Nothing
                                , FunctionExport "b"
                                ]
                        )
        , test "declaration exposing an infix operator" <|
            \() ->
                "module A exposing ((?))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ] <|
                            SubsetExport [ FunctionExport "?" ]
                        )
        , test "declaration exposing union" <|
            \() ->
                "module A exposing (A(..))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ] <|
                            SubsetExport [ TypeExport "A" (Just AllExport) ]
                        )
        , test "declaration exposing constructor subset" <|
            \() ->
                "module A exposing (A(A))"
                    |> isStatement
                        (ModuleDeclaration [ "A" ] <|
                            SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
                        )
        , test "multiline declaration" <|
            \() ->
                "module A exposing (A, B,\nc)"
                    |> isStatement
                        (ModuleDeclaration [ "A" ] <|
                            SubsetExport
                                [ TypeExport "A" Nothing
                                , TypeExport "B" Nothing
                                , FunctionExport "c"
                                ]
                        )
        , test "declaration using a port" <|
            \() ->
                "port module A exposing (A(..))"
                    |> isStatement
                        (PortModuleDeclaration [ "A" ] <|
                            SubsetExport [ TypeExport "A" (Just AllExport) ]
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
                        )
        ]


importStatements : Test
importStatements =
    describe "Import statements"
        [ test "simple import" <|
            \() -> "import A" |> isStatement (ImportStatement [ "A" ] Nothing Nothing)
        , test "import as" <|
            \() -> "import A as B" |> isStatement (ImportStatement [ "A" ] (Just "B") Nothing)
        , test "import exposing all" <|
            \() ->
                "import A exposing (..)"
                    |> isStatement (ImportStatement [ "A" ] Nothing (Just AllExport))
        , test "import exposing" <|
            \() ->
                "import A exposing (A, b)"
                    |> isStatement
                        (ImportStatement [ "A" ] Nothing <|
                            Just <|
                                SubsetExport
                                    [ TypeExport "A" Nothing
                                    , FunctionExport "b"
                                    ]
                        )
        , test "import exposing union" <|
            \() ->
                "import A exposing (A(..))"
                    |> isStatement
                        (ImportStatement [ "A" ] Nothing <|
                            Just <|
                                SubsetExport [ TypeExport "A" (Just AllExport) ]
                        )
        , test "import exposing constructor subset" <|
            \() ->
                "import A exposing (A(A))"
                    |> isStatement
                        (ImportStatement [ "A" ] Nothing <|
                            Just <|
                                SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
                        )
        , test "import multiline" <|
            \() ->
                "import A as B exposing (A, B,\nc)"
                    |> isStatement
                        (ImportStatement [ "A" ] (Just "B") <|
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
                    |> isStatement (FunctionTypeDeclaration "x" (TypeConstructor [ "Int" ] []))
        , test "variables" <|
            \() ->
                "x : a"
                    |> isStatement (FunctionTypeDeclaration "x" (TypeVariable "a"))
        , test "variables with numbers" <|
            \() ->
                "x : a1"
                    |> isStatement (FunctionTypeDeclaration "x" (TypeVariable "a1"))
        , test "application" <|
            \() ->
                "x : a -> b"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
                            (TypeApplication
                                (TypeVariable "a")
                                (TypeVariable "b")
                            )
                        )
        , test "application associativity" <|
            \() ->
                "x : a -> b -> c"
                    |> isStatement
                        (FunctionTypeDeclaration "x"
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
                    |> isStatement
                        (FunctionTypeDeclaration "x"
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
                    |> isStatement
                        (FunctionTypeDeclaration "m"
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
                    |> isStatement
                        (PortTypeDeclaration "focus"
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
                    |> isStatement
                        (PortTypeDeclaration "users"
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
                    |> isStatement
                        (PortDeclaration "focus"
                            []
                            (Access
                                (Variable [ "Cmd" ])
                                [ "none" ]
                            )
                        )
        ]


infixDeclarations : Test
infixDeclarations =
    describe "Infix declarations"
        [ test "non" <|
            \() ->
                "infix 9 :-"
                    |> isStatement (InfixDeclaration N 9 ":-")
        , test "left" <|
            \() ->
                "infixl 9 :-"
                    |> isStatement (InfixDeclaration L 9 ":-")
        , test "right" <|
            \() ->
                "infixr 9 :-"
                    |> isStatement (InfixDeclaration R 9 ":-")
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
                |> areStatements
                    [ FunctionTypeDeclaration "f"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , FunctionDeclaration "f"
                        [ Variable [ "x" ] ]
                        (Application
                            (Application
                                (var "a")
                                (RecordUpdate "r" [ ( "f", Integer 1 ) ])
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
                |> areStatements
                    [ FunctionTypeDeclaration "f"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , FunctionDeclaration "f"
                        [ Variable [ "x" ] ]
                        (BinOp
                            (Variable [ "+" ])
                            (Variable [ "x" ])
                            (Integer 1)
                        )
                    , FunctionTypeDeclaration "g"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , FunctionDeclaration "g"
                        [ Variable [ "x" ] ]
                        (BinOp
                            (Variable [ "+" ])
                            (Application
                                (Variable [ "f" ])
                                (Variable [ "x" ])
                            )
                            (Integer 1)
                        )
                    , FunctionTypeDeclaration "h"
                        (TypeApplication
                            (TypeTuple [ TypeConstructor [ "Int" ] [], TypeConstructor [ "Int" ] [] ])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , FunctionDeclaration "h"
                        [ Tuple [ Variable [ "a" ], Variable [ "b" ] ] ]
                        (BinOp
                            (Variable [ "+" ])
                            (Variable [ "a" ])
                            (Variable [ "b" ])
                        )
                    , FunctionTypeDeclaration "+"
                        (TypeApplication
                            (TypeConstructor [ "Int" ] [])
                            (TypeConstructor [ "Int" ] [])
                        )
                    , FunctionDeclaration "+" [ Variable [ "a" ], Variable [ "b" ] ] (Integer 1)
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
                    , InfixDeclaration L 1 "++"
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
                    , InfixDeclaration R 1 "**"
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
                emptyRecordAliasInput |> areStatements [ TypeAliasDeclaration (TypeConstructor [ "A" ] []) (TypeRecord []) ]
        , test "can parse aliases of unit" <|
            \() ->
                emptyTupleAliasInput |> areStatements [ TypeAliasDeclaration (TypeConstructor [ "A" ] []) (TypeTuple []) ]
        ]
