module Multiline exposing (application)

import Helpers exposing (isApplicationSansMeta, var)
import Test exposing (Test, describe, test)


application : Test
application =
    describe "Multiline performance"
        [ test "Application" <|
            \() ->
                "fn\n  a\n  b\n  c\n  d\n  e\n  f\n  g\n  h\n  i\n  j\n  k\n   i"
                    |> isApplicationSansMeta (var "fn")
                        [ var "a"
                        , var "b"
                        , var "c"
                        , var "d"
                        , var "e"
                        , var "f"
                        , var "g"
                        , var "h"
                        , var "i"
                        , var "j"
                        , var "k"
                        , var "i"
                        ]
        ]
