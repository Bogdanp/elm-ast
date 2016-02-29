module Main where

import Html exposing (..)
import Html.Events exposing (..)

import Ast

type Action
  = NoOp
  | Update String

init : String
init = """module Main where

f : Int -> Int
f x = x + 1

g : Int -> Int
g x = x * 2

h = f << g
"""

update : Action -> String -> String
update action model =
  case action of
    NoOp ->
      model

    Update m ->
      m

view : Signal.Address Action -> String -> Html
view address model =
  div [] [ textarea [ on "input" targetValue (Signal.message address << Update) ] [ text model ]
         , pre [] [ text <| toString <| Ast.parse model ]
         ]

mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoOp

main : Signal Html
main =
  Signal.foldp update init mailbox.signal
    |> Signal.map (view mailbox.address)
