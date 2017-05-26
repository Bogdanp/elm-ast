port module Main exposing (..)

import Expression
import Statement
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (Test, describe)


main : TestProgram
main =
    run emit all


port emit : ( String, Value ) -> Cmd msg


all : Test
all =
    describe "Ast suite"
        [ Expression.all
        , Statement.all
        ]
