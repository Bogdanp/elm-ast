module Parsing exposing (wrongWhitespace)

import Helpers exposing (..)
import Test exposing (Test, describe, test)


wrongWhitespaceInput : String
wrongWhitespaceInput =
    """
-- Empty line with spaces at the end here
b =
  2
""" ++ "  " ++ """
a =
  1
"""


wrongWhitespace : Test
wrongWhitespace =
    describe "Doesn't crash on wrong whitespace"
        [ test "Two spaces at the end of an empty line" <|
            \() ->
                wrongWhitespaceInput
                    |> areStatementsSansMeta
                        [ comment " Empty line with spaces at the end here"
                        , functionDeclaration (functionPattern "b" []) (integer 2)
                        , functionDeclaration (functionPattern "a" []) (integer 1)
                        ]
        ]
