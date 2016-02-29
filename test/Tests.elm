module Tests where

import ElmTest exposing (..)
import String

import Expression
import Statement


all : Test
all =
  suite "Ast suite" [ Expression.all
                    , Statement.all
                    ]
