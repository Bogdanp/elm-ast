module Main exposing (all)

import Test exposing (..)

import Expression
import Statement

all : Test
all =
  describe "Ast suite" [ Expression.all
                       , Statement.all
                       ]
