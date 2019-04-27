module Meta exposing (idGeneration)

import Ast.Expression exposing (generateIds)
import Expect exposing (..)
import Helpers exposing (..)
import Test exposing (Test, describe, test)


idGeneration : Test
idGeneration =
    let
        trivialCases =
            [ "'a'", "12", "5.6", "\"hello\"" ]

        -- TODO: fix this case
        --       complexCase = """
        -- let
        --   x = [1,2,3]
        --   y = (1,2,3)
        --   z = {a = 1, b = 2, c = 3}
        --   f = List.map (a -> a + 1)
        --   w = Just 5
        --   n = {z | c = 5}
        -- in
        --   if
        --     1 < 2
        --   then
        --     3
        --   else
        --     case w of
        --       Nothing -> n.c
        --       Just v -> f x
        -- """
        complexCase =
            """
let
  f x = x + 1
  g x = x + 1
in
  f 4
"""

        testGeneratedIds =
            \i () ->
                case simpleParse i of
                    Ok e ->
                        generateIds e |> hasUniqueIds

                    Err msg ->
                        Expect.fail msg
    in
    describe "Trivial id generation" <|
        List.map (\i -> test ("Trivial: " ++ i) (testGeneratedIds i)) trivialCases
            ++ [ test "Complex" (testGeneratedIds complexCase)
               ]
