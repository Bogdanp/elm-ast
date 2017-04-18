port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)

import Expression
import Statement

main : Test.Runner.Node.TestProgram
main =
    run emit all

port emit : ( String, Value ) -> Cmd msg

all : Test
all =
   describe "Ast suite" [ Expression.all
                        , Statement.all
                        ]
