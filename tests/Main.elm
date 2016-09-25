module Main exposing (..)

import Test exposing (..)
import String

import Expression
import Statement

suite : Test
suite =
  describe "Ast suite" [ Expression.suite
                       , Statement.suite
                       ]
